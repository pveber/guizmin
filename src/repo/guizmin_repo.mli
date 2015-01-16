type path = string list
type html_elt = [`Html] Html5.M.elt

module type Engine = sig
  val build : Guizmin.Workflow.u -> string Lwt.t
end

module Make(E : Engine) : sig
  type page
  type t

  val html_page : path -> (unit -> html_elt Lwt.t) -> page
  val file_page : ?path:path -> ?in_situ:bool -> _ Guizmin.Workflow.t -> page

  val href : page -> string
  val a : page -> 'a Html5.M.elt list -> [> `A of 'a] Html5.M.elt

  val empty : t
  val add_page : t -> page -> t
  val generate : dir:string -> t -> unit Lwt.t
end
