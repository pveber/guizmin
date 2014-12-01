open Core.Std
open Workflow.Types
open Workflow.API

let keep ~n bed =
  if n < 1 then raise (Invalid_argument "Bed.keep") ;
  workflow [
    program "cut" ~stdout:(target ()) [
      string (sprintf "1-%d" n) ;
      dep bed ;
    ]
  ]

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = unit bed3_like

let keep3 x = keep ~n:3 x




type 'a bed4_like = (string * 'a) bed3_like

type bed4 = unit bed4_like

let keep4 x = keep ~n:4 x




type 'a bed5_like = (float * 'a) bed4_like

type bed5 = unit bed5_like

let keep5 x = keep ~n:5 x
