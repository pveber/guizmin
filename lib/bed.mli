open Std

type 'a bed3_like = < columns : string * (int * (int * 'a)) ;
                      header : [`no] ;
                      comment : [`sharp] ; .. > tsv

type bed3 = unit bed3_like

val keep3 : 'a bed3_like workflow -> bed3 workflow


type 'a bed4_like = (string * 'a) bed3_like

type bed4 = unit bed4_like
val keep4: 'a bed4_like workflow -> bed4 workflow


type 'a bed5_like = (int * 'a) bed4_like
type bed5 = unit bed5_like
val keep5 : 'a bed5_like workflow -> bed5 workflow
