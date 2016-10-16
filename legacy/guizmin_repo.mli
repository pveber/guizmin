open Bistro.Std

type path = string list

module Make() : sig
  type 'a page

  val html_page :
    path ->
    Tyxml_html.doc ->
    [`html] page

  val file_page :
    ?path:path ->
    'a Bistro.Workflow.t ->
    [ `Ok of 'a page | `Error of error ] Lwt.t

  val path : _ page -> path
  val href : _ page -> string
  val href_in_dir :
    (_ directory as 'a) page ->
    ('a, 'b) Bistro.Workflow.selector ->
    string
  val a : _ page -> 'a Html5.M.elt list -> [> `A of 'a] Html5.M.elt
  val a_in_dir :
    (_ directory as 'a) page ->
    ('a, _) Bistro.Workflow.selector ->
    'e Html5.M.elt list ->
    [> `A of 'e] Html5.M.elt
end
