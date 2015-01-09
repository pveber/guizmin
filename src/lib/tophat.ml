open Core.Std
open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/tophat-install/2.0.13/tophat-install.sh"

let package = workflow [
    bash package_script [ dest ]
  ]

let tophat1 ?num_threads ?color index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow [
    program ~path:[package ; Bowtie.package ; Samtools.package] "tophat" [
      option (opt "--num-threads" int) num_threads ;
      option (flag string "--color") color ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let tophat2 ?num_threads index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow [
    program ~path:[package ; Bowtie2.package ; Samtools.package] "tophat2" [
      option (opt "--num-threads" int) num_threads ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let accepted_hits od =
  Workflow.extract od ["accepted_hits.bam"]

let junctions od =
  Workflow.extract od ["junctions.bed"]

