open Bistro
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"fastool" ~tag:"0.1.4" ()

let fastool fqgz =
  Workflow.shell ~descr:"fastool" [
    pipe [
      cmd "zcat" [ dep fqgz ] ;
      cmd "fastool" ~env ~stdout:dest [
        string "--illumina-trinity"
      ] ;
    ]
  ]
