open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/fastqc-install/0.10.1/fastqc-install.sh"

let package =
  workflow [
    bash package_script [ dest ]
  ]

type report
type workflow = report directory Workflow.t

let run fq = workflow [
    mkdir_p dest ;
    program "fastqc" ~path:[package] [
      seq [string "--outdir=" ; dest] ;
      dep fq ;
    ] ;
    rm_rf (dest // "*.zip") ;
    mv (dest // "*_fastqc/*") (dest) ;
    rm_rf (dest // "*_fastqc") ;
  ]


let html_report dir =
  Workflow.extract dir ["fastqc_report.html"]

let per_base_quality dir =
  Workflow.extract dir ["Images" ; "per_base_quality.png"]

let per_base_sequence_content dir =
  Workflow.extract dir ["Images" ; "per_base_sequence_content.png"]

