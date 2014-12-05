open Workflow.Types

val count :
  ?mode:[`union | `intersection_strict | `intersection_nonempty] ->
  ?stranded:[` yes | `no | `reverse] ->
  ?feature_type:string ->
  ?minaqual:int ->
  ?idattribute:string ->
  Sam.workflow -> Gff.file workflow ->
  < columns : string * (int * unit) ;
    header : [`no] ;
    comment : [`sharp] ; .. > tsv workflow
