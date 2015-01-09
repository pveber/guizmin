open Core.Std
open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/samtools-install/0.1.17/samtools-install.sh"

let package = workflow [
    bash package_script [ dest ]
  ]

let samtools cmd args = program "samtools" ~path:[package] (string cmd :: args)

let sam_of_bam bam =
  workflow [
    samtools "view" [
      opt "-o" ident dest ;
      dep bam ;
    ]
  ]

let indexed_bam_of_sam sam =
  workflow [
    mkdir_p dest ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> dest // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      dest // "temp.bam" ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
    rm_rf (dest // "temp.bam") ;
  ]

let sort ?on:order bam =
  workflow [
    samtools "sort" [
      option (fun o -> flag string "-n" (o = `name)) order ;
      dep bam ;
      dest ;
    ] ;
    mv (seq [dest ; string ".bam"]) dest ;
  ]

let indexed_bam_of_bam bam =
  workflow [
    mkdir_p dest ;
    samtools "sort" [
      dep bam ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
  ]

let bam_of_indexed_bam ibam =
  Workflow.extract ibam ["reads.bam"]
