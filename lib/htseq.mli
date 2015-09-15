open Std

type count_tsv =
    < columns : string * (int * unit) ;
    header : [`no] ;
    comment : [`sharp] ; .. > tsv

val count :
  ?order:[`name | `position] ->
  ?mode:[`union | `intersection_strict | `intersection_nonempty] ->
  ?stranded:[` yes | `no | `reverse] ->
  ?feature_type:string ->
  ?minaqual:int ->
  ?idattribute:string ->
  [`sam of Sam.workflow | `bam of Bam.workflow] ->
  Gff.file workflow ->
  count_tsv workflow
