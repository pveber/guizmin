open Bistro_std.Types

type path = string list
type html_elt = [`Html] Html5.M.elt

type error = [
    `Workflow_error of (Bistro.Workflow.u * string) list
  | `Failure of string
]

module type Engine = sig
  val root : string
  val build : Bistro.Workflow.u -> [ `Ok of string | `Error of error ] Lwt.t
end

module Make(E : Engine) : sig
  type 'a page

  val html_page :
    path ->
    html_elt ->
    [ `Ok of html page | `Error of error ] Lwt.t

  val file_page :
    ?path:path ->
    'a Bistro.Workflow.t ->
    [ `Ok of 'a page | `Error of error ] Lwt.t

  val path : _ page -> path
  val href : _ page -> string
  val a : _ page -> 'a Html5.M.elt list -> [> `A of 'a] Html5.M.elt
  val a_in_dir :
    _ directory page ->
    path ->
    'a Html5.M.elt list ->
    [ `Ok of [> `A of 'a] Html5.M.elt
    | `Error of [> `Failure of string ] ]
end
