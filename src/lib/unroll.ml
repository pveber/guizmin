open Core.Std

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

  let genomes = extract_unique (
    function
    | Model m -> Some m.model_genome
    | _ -> None
  )

  let model id = List.find_exn models ~f:(fun m -> m.model_id = id)

  end

