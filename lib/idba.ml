open Core
open Bistro
open Bistro_bioinfo
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"idba" ~tag:"1.1.3" ()

type fq2fa_input = [
  | `Se of sanger_fastq pworkflow
  | `Pe_merge of sanger_fastq pworkflow * sanger_fastq pworkflow
  | `Pe_paired of sanger_fastq pworkflow
]

let fq2fa ?filter:_ input = (* FIXME filter *)
  let args = match input with
    | `Se fq -> dep fq
    | `Pe_merge (fq1, fq2) ->
      opt "--merge" ident (seq ~sep:" " [dep fq1 ; dep fq2])
    | `Pe_paired fq ->
      opt "--paired" dep fq
  in
  Workflow.shell ~descr:"fq2fa" [
    cmd "fq2fa" ~env [
      args ;
      dest ;
    ]
  ]

let idba_ud ?(mem_spec = 10) fa =
  Workflow.shell ~np:4 ~mem:(Workflow.int (mem_spec * 1024)) ~descr:"idba_ud" [
    mkdir_p dest ;
    cmd "idba_ud" ~env [
      opt "--read" dep fa ;
      opt "--num_threads" ident np ;
      opt "--out" ident dest ;
    ]
  ]

let idba_ud_contigs x = Workflow.select x ["contig.fa"]
let idba_ud_scaffolds x = Workflow.select x ["scaffold.fa"]
