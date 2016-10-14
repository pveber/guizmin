type path = string list
type 'a build
type 'a page
type 'a link
type html
type 'a data = 'a Bistro_app.path

val pure  : 'a -> 'a build
val pureW : 'a Bistro.workflow -> 'a data build
val pureP : 'a page -> 'a link build
val app : ('a -> 'b) build -> 'a build -> 'b build
val list : 'a build list -> 'a list build
val option : 'a build option -> 'a option build

module Infix : sig
  val ( $ ) : ('a -> 'b) build -> 'a build -> 'b build
end

val html_page : path -> Tyxml_html.doc -> html page
val data_page : ?path:path -> 'a Bistro.workflow -> 'a page

val a : _ link -> 'a Tyxml_html.elt list -> [> `A of 'a] Tyxml_html.elt

val href : _ link -> string

val a_sel :
  'a Bistro.directory data link ->
  ('a, 'b) Bistro.selector ->
  'c Tyxml_html.elt list ->
  [> `A of 'c] Tyxml_html.elt

val generate : _ build -> dest:string -> unit Bistro_app.t

module Syntax : sig
  module Let_syntax : sig
    type 'a t = 'a build
    val map  : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end

(* module Make() : sig *)
(*   type html *)
(*   type 'a page *)

(*   val html_page : path -> Tyxml_html.doc -> html page *)
(*   val bistro_output : path -> 'a Bistro.workflow -> 'a page *)

(*   val a : _ page -> 'a Tyxml_html.elt list -> [> `A of 'a] Tyxml_html.elt *)
(*   val href : _ page -> string *)

(*   val generate : dest:string -> webroot:string -> unit Bistro_app.t *)
(* end *)


(* type 'a t *)

(* val empty : [> ] t *)

(* val add_page : 'a t -> 'a -> path -> Tyxml_html.doc -> 'a t *)
(* val add_file : 'a t -> 'a -> path -> string -> 'a t *)

(* val a : 'a t -> 'a -> 'b Tyxml_html.elt list -> [> `A of 'a] Tyxml_html.elt *)

(* val generate : 'a t -> dest:string -> unit *)

(* type 'a contents = *)
(*   | Html : Tyxml_html.doc -> [`Html] contents *)
(*   | Bistro_path : 'a Bistro_app.path -> 'a contents *)
(*   | Bistro_path_select : 'a Bistro_app.path * ('a, 'b) Bistro.selector -> 'b contents *)




(* module type Page = sig *)
(*   type 'a t *)

(*   val return : 'a contents -> 'a t *)

(*   val path : _ t -> path *)
(*   val contents : 'a t -> 'a contents *)
(* end *)

(* module Make(Page : Page) : sig *)
(*   type t *)
(*   val empty : t *)
(*   val add_page : t -> _ Page.t -> Tyxml_html.doc -> t *)
(* end *)
