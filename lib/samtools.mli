open Std

val package : package workflow

val sort :
  ?on:[`name | `position] ->
  Bam.workflow -> Bam.workflow
val indexed_bam_of_sam : Sam.workflow -> [ `indexed_bam ] directory workflow
val indexed_bam_of_bam : Bam.workflow -> [ `indexed_bam ] directory workflow
val bam_of_indexed_bam : [ `indexed_bam ] directory workflow -> Bam.workflow
(* val bam_of_sam : Sam.workflow -> Bam.workflow *)
val sam_of_bam : Bam.workflow -> Sam.workflow

(* val rmdup : ?single_end_mode:bool -> bam workflow -> bam workflow *)
