open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

let pe_args (ones, twos) =
  let opt side i x =
    opt (sprintf "--pe%d-%d" (i + 1) side) dep x
  in
  seq ~sep:" " (
    List.mapi ones ~f:(opt 1)
    @
    List.mapi twos ~f:(opt 2)
  )

type spades_output = [`spades_output] directory

let spades
    ?single_cell ?iontorrent
    ?pe
    ?(mem_spec = 10)
    ()
  : spades_output workflow
  =
  workflow ~np:4 ~mem:(mem_spec * 1024) ~descr:"spades" [
    mkdir_p dest ;
    cmd "spades.py" [
      option (flag string "--sc") single_cell ;
      option (flag string "--iontorrent") iontorrent ;
      opt "--threads" ident np ;
      opt "--memory" (fun m -> seq [ string "$((" ; mem ; string " / 1024))" ]) mem ;
      option pe_args pe ;
      opt "-o" ident dest ;
    ]
  ]

let contigs = selector ["contigs.fasta"]
let scaffolds = selector ["scaffolds.fasta"]
