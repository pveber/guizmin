open Core.Std
open Workflow.Types
open Unrolled_workflow

let unsafe_file_of_url url : 'a workflow =
  let source () =
    if String.is_prefix ~prefix:"http://" url || String.is_prefix ~prefix:"ftp://" url
    then Utils.wget url
    else Workflow.input url
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

  let project_name =
    List.find_map S.config_file ~f:(function
        | Project name -> Some name
        | _ -> None
      )
    |>
    Option.value ~default:"X"

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

(*   class sample (s : Experiment_description.sample) = object *)
(*     method repr = s *)
(*     method id = s.sample_id *)
(*     method _type = s.sample_type *)
(*     method experiment = s.sample_exp *)
(*     method model = model s.sample_model *)
(*     method condition = s.sample_condition *)
(*   end *)

(*   let sanger_fastq_of_url format url = *)
(*     let f x = Fastq.to_sanger x (unsafe_file_of_url url) in *)
(*     match format with *)
(*     | `fastq `sanger -> f Fastq.Sanger *)
(*     | `fastq `solexa -> f Fastq.Solexa *)
(*     | `fastq `phred64 -> f Fastq.Phred64 *)
(*     | `sra -> Sra.fastq_dump (unsafe_file_of_url url) *)

(*   class short_read_sample sample format = object (s) *)
(*     inherit sample sample *)
(*     method format : short_read_format = format *)
(*     method sanger_fastq = *)
(*       List.map sample.sample_files ~f:(sanger_fastq_of_url format) *)
(*     method fastQC_report = FastQC.run (Fastq.concat s#sanger_fastq) *)
(*   end *)

(*   class short_read_sample_with_reference_genome sample format g = object *)
(*     inherit short_read_sample sample format *)
(*     method reference_genome = genome g *)
(*   end *)

(*   class simply_mapped_dna_seq_sample sample format genome = *)
(*     object (s) *)
(*       inherit short_read_sample_with_reference_genome sample format genome *)

(*       method aligned_reads = *)
(*         let index = s#reference_genome#bowtie_index in *)
(*         let fqs = s#sanger_fastq in *)
(*         Bowtie.bowtie ~v:2 ~m:1 index fqs *)

(*       method aligned_reads_indexed_bam = *)
(*         Samtools.indexed_bam_of_sam s#aligned_reads *)

(*       method aligned_reads_bam = *)
(*         Samtools.bam_of_indexed_bam s#aligned_reads_indexed_bam *)
(*     end *)

(*   class tf_chip_seq_sample sample format genome tf = *)
(*     object (s) *)
(*       inherit simply_mapped_dna_seq_sample sample format genome *)
(*       method tf : string = tf *)
(*     end *)

(*   let samples = *)
(*     extract_unique ( *)
(*       function *)
(*       | Sample s -> Some s *)
(*       | _ -> None *)
(*     ) *)


(*   let short_read_sample sobj format = *)
(*     let s = sobj # repr in *)
(*     match sobj # model . model_genome with *)
(*     | Some g -> ( *)
(*         match s.sample_exp with *)
(*         | `TF_ChIP tf -> *)
(*           `TF_ChIP_seq (new tf_chip_seq_sample s format g tf) *)
(*         | `FAIRE -> *)
(*           `FAIRE_seq (new simply_mapped_dna_seq_sample s format g) *)
(*         | `EM_ChIP _ -> *)
(*           `EM_ChIP_seq (new simply_mapped_dna_seq_sample s format g) *)
(*         | `mRNA -> `mRNA_seq (new short_read_sample s format) *)
(*         | `whole_cell_extract -> `WCE_seq (new simply_mapped_dna_seq_sample s format g) *)
(*       ) *)
(*     | None -> *)
(*       `Short_read_sample (new short_read_sample s format) *)

(*   let any_sample s : any_sample = match s # _type with *)
(*     | `short_reads format -> short_read_sample s format *)

(*   let any_samples = List.map samples ~f:(fun s -> any_sample (new sample s)) *)

(*   let tf_chip_seq_samples = List.filter_map any_samples ~f:(function *)
(*       | `TF_ChIP_seq s -> Some s *)
(*       | `EM_ChIP_seq _ *)
(*       | `FAIRE_seq _ *)
(*       | `mRNA_seq _ *)
(*       | `WCE_seq _ *)
(*       | `Short_read_sample _ -> None *)
(*     ) *)

(*   let faire_seq_samples = List.filter_map any_samples ~f:(function *)
(*       | `FAIRE_seq s -> Some s *)
(*       | `TF_ChIP_seq _ *)
(*       | `EM_ChIP_seq _ *)
(*       | `mRNA_seq _ *)
(*       | `WCE_seq _ *)
(*       | `Short_read_sample _ -> None *)
(*     ) *)

(*   let mappable_short_read_samples = List.filter_map any_samples ~f:(function *)
(*       | `TF_ChIP_seq s -> Some (s :> mappable_short_read_sample) *)
(*       | `EM_ChIP_seq s -> Some (s :> mappable_short_read_sample) *)
(*       | `FAIRE_seq s -> Some (s :> mappable_short_read_sample) *)
(*       | `mRNA_seq s -> None *)
(*       | `WCE_seq s -> Some (s :> mappable_short_read_sample) *)
(*       | `Short_read_sample s -> None *)
(*     ) *)

(*   let short_read_samples = List.filter_map any_samples ~f:(function *)
(*       | `TF_ChIP_seq s -> Some (s :> short_read_sample) *)
(*       | `EM_ChIP_seq s -> Some (s :> short_read_sample) *)
(*       | `FAIRE_seq s -> Some (s :> short_read_sample) *)
(*       | `mRNA_seq s -> Some (s :> short_read_sample) *)
(*       | `WCE_seq s -> Some (s :> short_read_sample) *)
(*       | `Short_read_sample s -> Some (s :> short_read_sample) *)
(*     ) *)

(*   let sample_of_any = function *)
(*     | `TF_ChIP_seq s -> (s :> sample) *)
(*     | `EM_ChIP_seq s -> (s :> sample) *)
(*     | `FAIRE_seq s -> (s :> sample) *)
(*     | `WCE_seq s -> (s :> sample) *)
(*     | `mRNA_seq s -> (s :> sample) *)
(*     | `Short_read_sample s -> (s :> sample) *)

(*   let samples = List.map any_samples ~f:sample_of_any *)

end

let from_description ged =
  let module X = struct let config_file = ged end in
  let module W = Make(X) in
  (module W : Unrolled_workflow.S)
