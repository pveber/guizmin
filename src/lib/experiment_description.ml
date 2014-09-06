open Sexplib.Std

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


(* FIXME: REPLUG THESE CONSISTENCY CHECKS !!! (probably in some other module) *)

(* FIXME : test no more than one project tag *)

(* type error = [ *)
(*   | `duplicate_condition_id of string *)
(*   | `duplicate_sample_id of string *)
(* ] *)
(* with sexp *)

(* module Check : sig *)
(*   val errors : t -> error list *)
(*   val error_msg : error -> string *)
(* end *)
(* = struct *)
(*   let find_dups l = *)
(*     let rec aux seen dups = function *)
(*       | [] -> dups *)
(*       | h :: t -> *)
(*         if List.mem seen h then *)
(*           aux seen (h :: dups) t *)
(*         else *)
(*           aux (h :: seen) dups t *)
(*     in *)
(*     aux [] [] l *)

(*   let dup_conditions = find_dups conditions *)
(*   let dup_sample_ids = List.(map samples ~f:(fun s -> s.sample_id) |! find_dups) *)

(*   let errors = *)
(*     List.(concat [ *)
(*         map dup_conditions ~f:(fun x -> `duplicate_condition_id x) ; *)
(*         map dup_sample_ids ~f:(fun x -> `duplicate_sample_id x) ; *)
(*       ]) *)

(*   let error_msg = *)
(*     List.map errors ~f:(fun e -> Sexp.to_string_hum (sexp_of_error e)) *)
(* end *)
