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

