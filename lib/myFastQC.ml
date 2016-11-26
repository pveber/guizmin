open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"fastqc" ~tag:"0.11.5" ()

let run fq_gz =
  let tmp_fq = tmp // "temp.fq" in
  workflow ~descr:"fastQC" [
    mkdir_p dest ;
    cmd "zcat" ~stdout:tmp_fq [ dep fq_gz ] ;
    cmd "fastqc" ~env [
      seq ~sep:"" [ string "--outdir=" ; dest ] ;
      tmp_fq ;
    ]
  ]
