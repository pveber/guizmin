open Core.Std
open Workflow.Types
open Workflow.API

let ( % ) = Fn.compose

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/htseq-install/0.6.1p1/htseq-install.sh"

let package = workflow [
    bash package_script [ target () ]
  ]

let string_of_mode = function
  | `union -> "union"
  | `intersection_strict -> "intersection-strict"
  | `intersection_nonempty -> "intersection-nonempty"

let string_of_strandedness = function
  | `yes -> "yes"
  | `no -> "no"
  | `reverse -> "reverse"

let count ?mode ?stranded ?feature_type ?minaqual ?idattribute sam gff =
  workflow [
    program ~pythonpath:[package] ~path:[package] ~stdout:(target ()) "htseq-count" [
      option (opt "-m" (string % string_of_mode)) mode ;
      option (opt "-s" (string % string_of_strandedness)) stranded ;
      option (opt "-t" string) feature_type ;
      option (opt "-a" int) minaqual ;
      option (opt "-i" string) idattribute ;
      dep sam ;
      dep gff ;
    ]
  ]
