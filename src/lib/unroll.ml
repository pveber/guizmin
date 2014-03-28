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

    let sequence x = Ucsc_gb.genome_sequence x
  end

  module Short_read_sample = struct
    type t = sample * short_read_format
    let list =
      extract (
	  function
	  | Sample ({ sample_type = `short_reads format } as s) ->
	     Some (s, format)
	  | _ -> None
	)

    let short_read_format = snd

    let sanger_fastq_of_url format url =
      let f x = Fastq.to_sanger x (unsafe_file_of_url url) in
      match format with
      | `fastq `sanger -> f Fastq.Sanger
      | `fastq `solexa -> f Fastq.Solexa
      | `fastq `phred64 -> f Fastq.Phred64
      | `sra -> Sra.fastq_dump (unsafe_file_of_url url)

    let sanger_fastq (sample, format) =
      List.map sample.sample_files ~f:(sanger_fastq_of_url format)

    let fastQC_report x = List.map (sanger_fastq x) ~f:FastQC.run
  end

end

