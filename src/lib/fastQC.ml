open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/fastqc-install/0.10.1/fastqc-install.sh"

let package =
  workflow [
    bash package_script [ target () ]
  ]

type report
type workflow = report directory Workflow.t

let run fq = workflow [
    mkdir_p (target ()) ;
    program "fastqc" ~path:[package] [
      seq [string "--outdir=" ; target ()] ;
      dep fq ;
    ] ;
    rm_rf (target () // "*.zip") ;
    mv (target () // "*_fastqc/*") (target ()) ;
    rm_rf (target () // "*_fastqc") ;
  ]


let html_report dir =
  Workflow.extract dir ["fastqc_report.html"]

let per_base_quality dir =
  Workflow.extract dir ["Images" ; "per_base_quality.png"]

let per_base_sequence_content dir =
  Workflow.extract dir ["Images" ; "per_base_sequence_content.png"]

