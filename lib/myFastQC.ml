open Bistro
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"fastqc" ~tag:"0.11.5" ()

let run fq_gz =
  let tmp_fq = tmp // "temp.fq" in
  Workflow.shell ~descr:"fastQC" [
    mkdir_p dest ;
    cmd "zcat" ~stdout:tmp_fq [ dep fq_gz ] ;
    cmd "fastqc" ~env [
      seq ~sep:"" [ string "--outdir=" ; dest ] ;
      tmp_fq ;
    ]
  ]
