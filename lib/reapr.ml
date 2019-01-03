open Bistro
open Bistro.Shell_dsl

let reapr (fq_1, fq_2) assembly =
  let bam = seq ~sep:"/" [ tmp ; string "mapped.bam" ] in
  Workflow.shell ~descr:"reapr" [
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

let assembly x = Workflow.select x ["04.break.broken_assembly.fa"]
