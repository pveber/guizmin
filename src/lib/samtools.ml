open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/samtools-install/0.1.17/samtools-install.sh"

let package = workflow [
    bash package_script [ target () ]
  ]

let samtools cmd args = program "samtools" ~path:[package] (string cmd :: args)

let sam_of_bam bam =
  workflow [
    samtools "view" [
      opt "-o" target () ;
      dep bam ;
    ]
  ]

let indexed_bam_of_sam sam =
  workflow [
    mkdir_p (target ()) ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> target () // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      target () // "temp.bam" ;
      target () // "reads" ;
    ] ;
    samtools "index" [ target () // "reads.bam" ] ;
    rm_rf (target () // "temp.bam") ;
  ]

let indexed_bam_of_bam bam =
  workflow [
    mkdir_p (target ()) ;
    samtools "sort" [
      dep bam ;
      target () // "reads" ;
    ] ;
    samtools "index" [ target () // "reads.bam" ] ;
  ]

let bam_of_indexed_bam ibam =
  Workflow.extract ibam ["reads.bam"]
