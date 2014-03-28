open Bistro_types

val wget : string -> 'a file workflow
val gunzip : 'a gz workflow -> 'a file workflow
val unzip : 'a zip workflow -> 'a directory workflow
val tar_xfz : 'a tgz workflow -> 'a workflow
val crlf2lf : 'a file workflow -> 'a file workflow
