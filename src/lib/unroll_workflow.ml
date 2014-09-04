open Core.Std
open Bistro_workflow.Types
open Unrolled_workflow

let unsafe_file_of_url url : 'a workflow =
  let source () =
    if String.is_prefix ~prefix:"http://" url || String.is_prefix ~prefix:"ftp://" url
    then Utils.wget url
    else Bistro_workflow.input url
  in
  if Filename.check_suffix url ".gz"
  then Utils.gunzip (source ())
  else source ()

module type Settings =
  sig
    val config_file : Experiment_description.t
  end

module Make(S : Settings) = struct
  open Experiment_description

  let extract f = List.filter_map S.config_file ~f
  let extract_unique f = List.dedup (extract f)

  let conditions = extract_unique (
    function
    | Condition c -> Some c
    | _ -> None
  )

  let models =
    extract_unique (
      function
      | Model m -> Some m
      | _ -> None
    )

  let model id = List.find_exn models ~f:(fun m -> m.model_id = id)

  class genome g = object (s)
    method repr : Experiment_description.genome = g
    method sequence = match g with
      | `ucsc x -> Ucsc_gb.genome_sequence x
      | `fasta url -> unsafe_file_of_url url
    method bowtie_index =
      Bowtie.bowtie_build s#sequence
  end

  let genomes = extract_unique (
      function
      | Model m -> Option.map ~f:(new genome) m.model_genome
      | _ -> None
    )

  let genome g = List.find_exn genomes ~f:(fun x -> x # repr = g)

  class sample (s : Experiment_description.sample) = object
    method repr = s
    method id = s.sample_id
    method _type = s.sample_type
    method experiment = s.sample_exp
    method model = model s.sample_model
    method condition = s.sample_condition
  end

  let sanger_fastq_of_url format url =
    let f x = Fastq.to_sanger x (unsafe_file_of_url url) in
    match format with
    | `fastq `sanger -> f Fastq.Sanger
    | `fastq `solexa -> f Fastq.Solexa
    | `fastq `phred64 -> f Fastq.Phred64
    | `sra -> Sra.fastq_dump (unsafe_file_of_url url)

  class short_read_sample sample format = object (s)
    inherit sample sample
    method format : short_read_format = format
    method sanger_fastq =
      List.map sample.sample_files ~f:(sanger_fastq_of_url format)
    method fastQC_report = List.map s#sanger_fastq ~f:FastQC.run
  end

  class short_read_sample_with_reference_genome sample format g = object
    inherit short_read_sample sample format
    method reference_genome = genome g
  end

  class simply_mapped_dna_seq_sample sample format genome =
    object (s)
      inherit short_read_sample_with_reference_genome sample format genome

      method aligned_reads =
        let index = s#reference_genome#bowtie_index in
        let fqs = s#sanger_fastq in
        Bowtie.bowtie ~v:2 ~m:1 index fqs

      method aligned_reads_indexed_bam =
        Samtools.indexed_bam_of_sam s#aligned_reads

      method aligned_reads_bam =
        Samtools.bam_of_indexed_bam s#aligned_reads_indexed_bam
    end

  class tf_chip_seq_sample sample format genome tf =
    object (s)
      inherit simply_mapped_dna_seq_sample sample format genome
      method tf : string = tf
    end

  let samples =
    extract_unique (
      function
      | Sample s -> Some s
      | _ -> None
    )
    |> List.map ~f:(new sample)

  let short_read_sample s = match s # repr with
    | { sample_type = `short_reads format } as s ->
      Some (new short_read_sample s format)

  let short_read_samples = List.filter_map samples ~f:short_read_sample

  let short_read_sample_with_reference_genome x =
    Option.map x#model.model_genome ~f:(
      new short_read_sample_with_reference_genome x#repr x#format
    )

  let short_read_samples_with_reference_genome =
    List.filter_map short_read_samples ~f:short_read_sample_with_reference_genome

  let tf_chip_seq_sample x =
    match x # _type, x # experiment with
    | `short_reads format, `TF_ChIP tf ->
      Some (new tf_chip_seq_sample x#repr format x#reference_genome#repr tf)
    | _ -> None

  let tf_chip_seq_samples =
    List.filter_map short_read_samples_with_reference_genome ~f:tf_chip_seq_sample


  let faire_seq_sample x =
    match x # _type, x # experiment with
    | `short_reads format, `FAIRE ->
      Some (new simply_mapped_dna_seq_sample x#repr format x#reference_genome#repr)
    | _ -> None

  let faire_seq_samples =
    List.filter_map short_read_samples_with_reference_genome ~f:faire_seq_sample

  let mappable_short_read_samples =
    let inj x = (x :> Unrolled_workflow.mappable_short_read_sample list) in
    let ( @ ) x y = inj x @ inj y in
    tf_chip_seq_samples @ faire_seq_samples (* FIXME @ others ! *)

 end

let from_description ged =
  let module X = struct let config_file = ged end in
  let module W = Make(X) in
  (module W : Unrolled_workflow.S)
