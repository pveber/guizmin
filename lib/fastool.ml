open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"fastool" ~tag:"0.1.4" ()

let fastool fqgz =
  workflow ~descr:"fastool" [
    pipe [
      cmd "zcat" [ dep fqgz ] ;
      cmd "fastool" ~stdout:dest [
        string "--illumina-trinity"
      ] ;
    ]
  ]
