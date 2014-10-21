module Future : sig
open Workflow.Types

val wget : string -> (_,_) file workflow
val gunzip : 'a gz workflow -> 'a workflow
val unzip : 'a zip workflow -> 'a workflow
val tar_xfz : 'a tgz workflow -> 'a workflow
val crlf2lf : (_,[`text]) file workflow -> (_,[`text]) file workflow
end

open Bistro_workflow.Types

val wget : string -> (_,_) file workflow
val gunzip : 'a gz workflow -> 'a workflow
val unzip : 'a zip workflow -> 'a workflow
val tar_xfz : 'a tgz workflow -> 'a workflow
val crlf2lf : (_,[`text]) file workflow -> (_,[`text]) file workflow
