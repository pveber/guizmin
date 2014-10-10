open Core.Std

type t = statement list
and statement =
| Condition of condition
| Sample of sample
| Model of model
| Project of string
and condition = string
and sample = {
  sample_id : string ;
  sample_type : sample_type ;
  sample_exp : experiment ;
  sample_files : string list ;
  sample_model : string ;
  sample_condition : string ;
}
and sample_type = [
| `short_reads of short_read_format
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_format = [
| `fastq of [ `sanger | `solexa | `phred64 ]
| `sra
]
and model = {
  model_id : string ;
  model_genome : genome option ;
}
and genome = [
| `ucsc of Ucsc_gb.genome
| `fasta of string
]
with sexp

let load path =
  Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp

let save cfg path =
  Sexplib.Sexp.save_hum path (sexp_of_t cfg)

type error = [
  | `multiple_declaration of [`condition | `model | `sample] * string
  | `missing_project_description
  | `more_than_one_project_description of string list
]
with sexp

module Check(X : sig
               val config : t
             end) = struct
  open X

  let extract f = List.filter_map config ~f
  let extract_unique f = List.dedup (extract f)
  let find_dups l =
    let rec aux seen dups = function
      | [] -> dups
      | h :: t ->
        if List.mem seen h then
          aux seen (h :: dups) t
        else
          aux (h :: seen) dups t
    in
    aux [] [] l


  let projects =
    extract (function
        | Project name -> Some name
        | _ -> None
      )

  let conditions = extract (
    function
    | Condition c -> Some c
    | _ -> None
  )

  let models =
    extract (
      function
      | Model m -> Some m
      | _ -> None
    )

  let samples =
    extract (
      function
      | Sample s -> Some s
      | _ -> None
    )

  let multiply_declared_conditions = find_dups conditions
  let multiply_declared_samples = List.(map samples ~f:(fun s -> s.sample_id) |! find_dups)
  let multiply_declared_models = List.(map models ~f:(fun s -> s.model_id) |! find_dups)
  let project_declaration_error = match projects with
    | [] -> [ `missing_project_description ]
    | [ _ ] -> []
    | ids -> [ `more_than_one_project_description ids ]

end

let check desc =
  let module E = Check(struct let config = desc end) in
  List.(concat [
      map E.multiply_declared_conditions ~f:(fun x -> `multiple_declaration (`condition, x)) ;
      map E.multiply_declared_models ~f:(fun x -> `multiple_declaration (`model, x)) ;
      map E.multiply_declared_samples ~f:(fun x -> `multiple_declaration (`sample, x)) ;
      E.project_declaration_error ;
    ])

let error_msg e =
  Sexp.to_string_hum (sexp_of_error e)
