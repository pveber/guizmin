open Core.Std
open Workflow.API

let wget ?no_check_certificate url =
  workflow [
    program "wget" [
      option (flag string "--no-check-certificate") no_check_certificate ;
      opt "-O" target () ; string url ]
  ]

let unzip zip =
  workflow [
    program "unzip" [ opt "-d" target () ; dep zip ]
  ]

let gunzip gz =
  workflow [
    program "gunzip" [ opt "-c" dep gz ] ~stdout:(target ())
  ]

let tar_xfz tgz =
  workflow [
    mkdir_p (target ()) ;
    program "tar" [ string "xfz" ; dep tgz ; opt "-C" target () ] ;
  ]

let crlf2lf f =
  workflow [
    program "tr" [ opt "-d" string "'\r'"] ~stdin:(dep f) ~stdout:(target ())
  ]
