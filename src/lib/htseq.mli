open Workflow.Types

type count_tsv =
    < columns : string * (int * unit) ;
    header : [`no] ;
    comment : [`sharp] ; .. > tsv

val count :
  ?mode:[`union | `intersection_strict | `intersection_nonempty] ->
  ?stranded:[` yes | `no | `reverse] ->
  ?feature_type:string ->
  ?minaqual:int ->
  ?idattribute:string ->
  Sam.workflow -> Gff.file workflow ->
  count_tsv workflow
