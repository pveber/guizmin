open Core.Std
open Bistro_types

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

  let samples = extract_unique (
    function
    | Sample s -> Some s
    | _ -> None
  )

  let models =
    extract_unique (
      function
      | Model m -> Some m
      | _ -> None
    )

  let model id = List.find_exn models ~f:(fun m -> m.model_id = id)

  module Genome = struct
    type t = genome

    let list =
      extract_unique (
	  function
	  | Model m -> Some m.model_genome
	  | _ -> None
	)

    let sequence = function
      | `ucsc x -> Ucsc_gb.genome_sequence x
      | `fasta url -> unsafe_file_of_url url

    let bowtie_index x =
      Bowtie.bowtie_build (sequence x)

  end

  module Short_read_sample = struct
    class type t = object
		     method sample : sample
		     method format : short_read_format
		   end

    let list =
      extract (
	  function
	  | Sample ({ sample_type = `short_reads format } as s) ->
	     Some (object method sample = s method format = format end)
	  | _ -> None
	)

    let sanger_fastq_of_url format url =
      let f x = Fastq.to_sanger x (unsafe_file_of_url url) in
      match format with
      | `fastq `sanger -> f Fastq.Sanger
      | `fastq `solexa -> f Fastq.Solexa
      | `fastq `phred64 -> f Fastq.Phred64
      | `sra -> Sra.fastq_dump (unsafe_file_of_url url)

    let sanger_fastq s =
      List.map s#sample.sample_files ~f:(sanger_fastq_of_url s#format)

    let fastQC_report x = List.map (sanger_fastq x) ~f:FastQC.run
  end

  module DNA_seq_with_reference = struct

    type exp = [ `TF_ChIP of string | `FAIRE | `whole_cell_extract]

    class type [+'a] t = object
			  constraint 'a = [< `TF_ChIP of string | `FAIRE | `whole_cell_extract]
			  inherit Short_read_sample.t
			  method experiment : 'a
			  method genomic_reference : Genome.t
			end

    let list = List.filter_map Short_read_sample.list ~f:(fun x ->
      match (x # sample).sample_exp with
      | `TF_ChIP _ | `FAIRE | `whole_cell_extract as exp ->
	     Some (object
		      method sample = x#sample
		      method format = x#format
		      method experiment = exp
		      method genomic_reference = (model (x # sample).sample_model).model_genome
		    end)
      | _ -> None
    )

    let aligned_reads x =
      Bowtie.bowtie ~v:2 ~m:1 (Genome.bowtie_index x#genomic_reference) (Short_read_sample.sanger_fastq x)

    let aligned_reads_indexed_bam x =
      Samtools.indexed_bam_of_sam (aligned_reads x)

    let aligned_reads_bam x = Samtools.bam_of_indexed_bam (aligned_reads_indexed_bam x)
  end

  module FAIRE_seq = struct
    type t = [`FAIRE] DNA_seq_with_reference.t

  end
end

let from_description ged =
  let module X = struct let config_file = ged end in
  let module W = Make(X) in
  (module W : Unrolled_workflow.S)
