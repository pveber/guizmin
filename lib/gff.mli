open Bistro_std

type file = < columns : string * (string * (string * (int * (int * (float * (string * (string * (string * unit)))))))) ;
              header : [`no] ;
              comment : [`sharp] ; .. > tsv
