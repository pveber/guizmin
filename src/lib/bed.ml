open Workflow.Types
open Workflow.API

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = unit bed3_like

let keep3 bed =
  workflow [
    program "cut" ~stdout:(target ()) [
      opt "-f" string "1-3" ;
      dep bed ;
    ]
  ]

type 'a bed4_like = (string * 'a) bed3_like

type bed4 = unit bed4_like
