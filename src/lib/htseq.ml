open Core.Std
open Workflow.Types
open Workflow.API

let ( % ) = Fn.compose

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/htseq-install/0.6.1p1/htseq-install.sh"

let package = workflow [
    bash package_script [ target () ]
  ]

type count_tsv =
    < columns : string * (int * unit) ;
    header : [`no] ;
    comment : [`sharp] ; .. > tsv

let string_of_mode = function
  | `union -> "union"
  | `intersection_strict -> "intersection-strict"
  | `intersection_nonempty -> "intersection-nonempty"

let string_of_strandedness = function
  | `yes -> "yes"
  | `no -> "no"
  | `reverse -> "reverse"

let string_of_order = function
  | `name -> "name"
  | `position -> "pos"

let count ?order ?mode ?stranded ?feature_type ?minaqual ?idattribute alns gff =
  let format, input = match alns with
    | `sam sam -> "sam", dep sam
    | `bam bam -> "bam", dep bam
  in
  workflow [
    program ~pythonpath:[package] ~path:[package] ~stdout:(target ()) "htseq-count" [
      opt "-f" string format ;
      option (opt "-m" (string % string_of_mode)) mode ;
      option (opt "-r" (string % string_of_order)) order ;
      option (opt "-s" (string % string_of_strandedness)) stranded ;
      option (opt "-t" string) feature_type ;
      option (opt "-a" int) minaqual ;
      option (opt "-i" string) idattribute ;
      input ;
      dep gff ;
    ]
  ]
