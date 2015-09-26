open Bistro_std.Types

type path = string list
type html_elt = [`Html] Html5.M.elt

module type Engine = sig
  val build : Bistro.Workflow.u -> [ `Ok of string
                                   | `Error of (Bistro.Workflow.u * string) list] Lwt.t
end

module Make(E : Engine) : sig
  type page

  val html_page : path -> html_elt -> page
  val file_page : ?path:path -> ?in_situ:bool -> _ Bistro.Workflow.t -> page

  val path : page -> path
  val href : page -> string
  val a : page -> 'a Html5.M.elt list -> [> `A of 'a] Html5.M.elt

  val generate : string -> [ `Ok of unit
                           | `Error of (Bistro.Workflow.u * string) list] Lwt.t
end
