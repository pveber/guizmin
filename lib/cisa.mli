open Bistro
open Bistro_bioinfo

val merge :
  ?min_length:int ->
  (string * fasta pworkflow) list -> fasta pworkflow

val cisa :
  int ->
  fasta pworkflow ->
  fasta pworkflow

(*   ?single_cell:bool -> *)
(*   ?iontorrent:bool -> *)
(*   ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list -> *)
(*   unit -> *)
(*   cisa_output workflow *)

(* val contigs : (cisa_output, fasta) selector *)
(* val scaffolds : (cisa_output, fasta) selector *)
