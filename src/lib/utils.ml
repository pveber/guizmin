module Future = struct

  open Workflow.API

let wget url =
  workflow [
    program "wget" [ opt "-O" target () ; string url ]
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
end

let wget url = Bistro_workflow.(
  make <:script< wget -O #DEST #s:url# >>
)

let unzip zip = Bistro_workflow.(
  make <:script<
    unzip -d #DEST #w:zip#
  >>
)

let gunzip gz = Bistro_workflow.(
  make <:script<
    gunzip -c #w:gz# > #DEST
  >>
)

let tar_xfz tgz = Bistro_workflow.(
  make <:script<
    mkdir -p #DEST
    tar xfz #w:tgz# -C #DEST
  >>
)

let crlf2lf f = Bistro_workflow.(
  make <:script<
    tr -d '\r' < #w:f# > #DEST
  >>
)

