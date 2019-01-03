open Core
open Bistro
open Bistro.Shell_dsl

let pe_args (ones, twos) =
  let opt side i x =
    opt (sprintf "--pe%d-%d" (i + 1) side) dep x
  in
  seq ~sep:" " (
    List.mapi ones ~f:(opt 1)
    @
    List.mapi twos ~f:(opt 2)
  )

let spades
    ?single_cell ?iontorrent
    ?pe
    ?(mem_spec = 10)
    ()
  =
  Workflow.shell ~np:4 ~mem:(Workflow.int (mem_spec * 1024)) ~descr:"spades" [
    mkdir_p dest ;
    cmd "spades.py" [
      option (flag string "--sc") single_cell ;
      option (flag string "--iontorrent") iontorrent ;
      opt "--threads" ident np ;
      opt "--memory" (fun m -> seq [ string "$((" ; m ; string " / 1024))" ]) mem ;
      option pe_args pe ;
      opt "-o" ident dest ;
    ]
  ]

let contigs x = Workflow.select x ["contigs.fasta"]
let scaffolds x = Workflow.select x ["scaffolds.fasta"]
