open Workflow.Types
open Workflow.API

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
    program ~path:[Bowtie.package ; Samtools.package] "tophat" [
      option (opt "--num-threads" int) num_threads ;
      option (flag string "--color") color ;
      dep index ;
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
    program ~path:[Bowtie2.package ; Samtools.package] "tophat2" [
      option (opt "--num-threads" int) num_threads ;
      dep index ;
      args
    ]
  ]

let accepted_hits od =
  Workflow.extract od ["accepted_hits.bam"]

let junctions od =
  Workflow.extract od ["junctions.bed"]

