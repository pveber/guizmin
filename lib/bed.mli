open Std

type closed

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = closed bed3_like

val keep3 : 'a bed3_like workflow -> bed3 workflow


type 'a bed4_like = (string * 'a) bed3_like

type bed4 = closed bed4_like
val keep4: 'a bed4_like workflow -> bed4 workflow


type 'a bed5_like = (int * 'a) bed4_like
type bed5 = closed bed5_like
val keep5 : 'a bed5_like workflow -> bed5 workflow
