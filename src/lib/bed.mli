open Workflow.Types

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = unit bed3_like

val keep3 : 'a bed3_like workflow -> bed3 workflow


type 'a bed4_like = (string * 'a) bed3_like

type bed4 = unit bed4_like
