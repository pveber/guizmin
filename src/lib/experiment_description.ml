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
  sample_data : sample_data ;
  sample_exp : experiment ;
  sample_model : string ;
  sample_condition : string ;
}
and sample_data = [
| `short_read_data of short_read_data
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `EM_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_data = [
| `fastq of
    [ `sanger | `solexa | `phred64 ] *
    string list se_or_pe
| `sra of
    [`single_end | `paired_end] *
    [ `SRR of string list | `file of string list ]
]
and model = {
  model_id : string ;
  model_genome : genome option ;
}
and genome = [
| `ucsc of Ucsc_gb.genome
| `fasta of string
]
and 'a se_or_pe = [
  | `single_end of 'a
  | `paired_end of 'a * 'a
]
with sexp

let se_or_pe_map x ~f = match x with
  | `single_end x -> `single_end (f x)
  | `paired_end (x_1, x_2) -> `paired_end (f x_1, f x_2)

let load path =
  Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp

let save cfg path =
  Sexplib.Sexp.save_hum path (sexp_of_t cfg)

type error = [
  | `undeclared of [`condition | `model | `sample] * string
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

  let model_ids = List.map models ~f:(fun m -> m.model_id)

  let samples =
    extract (
      function
      | Sample s -> Some s
      | _ -> None
    )

  let sample_ids = List.map samples ~f:(fun s -> s.sample_id)

  let undeclared item xs x = if List.mem xs x then None else (Some (`undeclared (item, x)))

  let undeclared_conditions =
    List.filter_map samples ~f:(fun s -> undeclared `condition conditions s.sample_condition)

  let undeclared_models =
    List.filter_map samples ~f:(fun s -> undeclared `model model_ids s.sample_model)

  let multiply_declared_conditions = find_dups conditions
  let multiply_declared_samples = find_dups sample_ids
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
      E.undeclared_conditions ;
      E.undeclared_models ;
    ])

let error_msg e =
  Sexp.to_string_hum (sexp_of_error e)
