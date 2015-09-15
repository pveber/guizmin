open Core.Std
open Bistro_std
open Bistro.EDSL_sh

let keep ~n bed =
  if n < 1 then raise (Invalid_argument "Bed.keep") ;
  workflow ~descr:"bed.keep" [
    cmd "cut" ~stdout:dest [
      string (sprintf "-f 1-%d" n) ;
      dep bed ;
    ]
  ]

type closed

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = closed bed3_like

let keep3 x = keep ~n:3 x




type 'a bed4_like = (string * 'a) bed3_like

type bed4 = closed bed4_like

let keep4 x = keep ~n:4 x




type 'a bed5_like = (int * 'a) bed4_like

type bed5 = closed bed5_like

let keep5 x = keep ~n:5 x
