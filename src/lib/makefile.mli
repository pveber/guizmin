type path = string list

type rule = {
  target : path ;
  deps : path list ;
  body : string list ;
  phony : bool ;
  desc : string option ;
}

type t = rule list

val empty : t
val add_rule : ?first:bool -> t -> rule -> t
val add_workflow : ?first:bool -> t -> _ Workflow.t -> t
