open Bistro.Std
open Bistro_bioinfo.Std

type cisa_output = [`cisa_output] directory

val merge :
  ?min_length:int ->
  (string * fasta workflow) list -> fasta workflow

val cisa :
  int ->
  fasta workflow ->
  fasta workflow

(*   ?single_cell:bool -> *)
(*   ?iontorrent:bool -> *)
(*   ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list -> *)
(*   unit -> *)
(*   cisa_output workflow *)

(* val contigs : (cisa_output, fasta) selector *)
(* val scaffolds : (cisa_output, fasta) selector *)
