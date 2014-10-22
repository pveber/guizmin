open Defs

type rule = {
  target : path ;
  deps : path list ;
  body : string list ;
  phony : bool ;
  desc : string option ;
}

type t = private rule list

val empty : t
val add_rule : t -> rule -> t
val add_workflow : t -> _ Workflow.t -> t
val ( ++ ) : t -> _ Workflow.t -> t

val to_channel : t -> out_channel -> unit
