(** Representation of a static website *)

type path = string list
type html_elt = [`Html] Html5.M.elt

module type S = sig
  type 'a page

  val path : _ page -> path

  val href : _ page -> string

  val a : _ page -> 'a Html5.M.elt list -> [`A of 'a] Html5.M.elt

  val html_page :
    path ->
    ?f:(unit -> html_elt Lwt.t) ->
    unit -> html_elt page

  val file_page : ?path:path -> _ Workflow.t -> Workflow.u page

  val register : html_elt page -> (unit -> html_elt Lwt.t) -> unit

  val generate :
    workflow_output:(Workflow.u -> string Lwt.t) ->
    output_dir:string ->
    unit Lwt.t
end

module Make(X : sig end) : S

