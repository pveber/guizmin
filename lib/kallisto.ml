open Core
open Bistro
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"kallisto" ~tag:"0.43.0" ()

type kallisto_output

let index fas =
  Workflow.shell ~descr:"kallisto-index" [
    cmd "kallisto index" ~env [
      opt "-i" ident dest ;
      list ~sep:" " dep fas ;
    ]
  ]

let quant ?bootstrap_samples idx fq1 fq2 =
  Workflow.shell ~descr:"kallisto-quant" ~np:4 [
    cmd "kallisto quant" ~env [
      opt "-i" dep idx ;
      opt "-o" ident dest ;
      opt "-t" ident np ;
      option (opt "-b" int) bootstrap_samples ;
      Utils.psgunzip fq1 ;
      Utils.psgunzip fq2 ;
    ]
  ]

let abundance x =
  Workflow.select x [ "abundance.tsv" ]
