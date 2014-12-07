open Workflow.Types
open Workflow.API

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

let sanger = Sanger
let solexa = Solexa
let phred64 = Phred64


type 'a workflow = ([`fastq of 'a format], [`text]) file Workflow.t

let sanger_of_solexa fq = assert false

let sanger_of_phred64 fq = assert false

let to_sanger (type s) (format : s format) (fq : s workflow) : [`sanger] workflow=
  match format with
  | Sanger -> fq
  | Solexa -> sanger_of_solexa fq
  | Phred64 -> sanger_of_phred64 fq

let concat = function
  | [] -> raise (Invalid_argument "Fastq.concat: empty list")
  | x :: [] -> x
  | fqs ->
    workflow [
      program "cat" [ list dep ~sep:" " fqs ] ~stdout:(target ())
    ]


(* open Bistro_workflow.Types *)

(* type _ format = *)
(* | Sanger : [`sanger] format *)
(* | Solexa : [`solexa] format *)
(* | Phred64 : [`phred64] format *)

(* let sanger = Sanger *)
(* let solexa = Solexa *)
(* let phred64 = Phred64 *)


(* type 'a workflow = ([`fastq of 'a format], [`text]) file Bistro_workflow.t *)

(* let sanger_of_solexa fq = assert false *)

(* let sanger_of_phred64 fq = assert false *)

(* let to_sanger (type s) (format : s format) (fq : s workflow) : [`sanger] workflow= *)
(*   match format with *)
(*   | Sanger -> fq *)
(*   | Solexa -> sanger_of_solexa fq *)
(*   | Phred64 -> sanger_of_phred64 fq *)

(* let concat fqs = Bistro_workflow.make <:script< *)

(* cat #! fq <- fqs #[#w:fq#][ ] > #DEST *)

(* >> *)
