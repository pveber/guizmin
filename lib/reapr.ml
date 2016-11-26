open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL


type output

let reapr (fq_1, fq_2) assembly =
  let bam = seq ~sep:"/" [ tmp ; string "mapped.bam" ] in
  workflow ~descr:"reapr" [
    mkdir_p tmp ;
    cmd "reapr" [
      string "smaltmap" ;
      dep assembly ;
      dep fq_1 ;
      dep fq_2 ;
      bam ;
    ] ;
    cmd "reapr" [
      string "pipeline" ;
      dep assembly ;
      bam ;
      dest ;
    ]
  ]

let assembly = selector ["04.break.broken_assembly.fa"]
