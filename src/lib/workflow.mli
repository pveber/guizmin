type path = string list

type 'a t = private u
and u = {
  target : path ;
  deps : u list ;
  script : cmd list ;
}
and cmd = token list
and token =
  | S : string -> token
  | D : _ t -> token
  | T : token

module Types : sig
  type 'a workflow = 'a t

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type package = [`package] directory

  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file

  class type ['a, 'b, 'c, 'd] tabular = object
    inherit [[`tabular], [`text]] file
    method columns : 'a
    method header : [< `yes | `no] as 'b
    method sep : 'c
    method comment : 'd
  end

  type ('a, 'b, 'c) tsv = ('a, 'b, [`tab], 'c) tabular

end

open Types

val string_of_cmd : path -> cmd -> string

val make : ?target:path -> cmd list -> 'a t
val in_target : _ directory t -> path -> 'a t
val input : path -> 'a t

val ( >:: ) : path -> 'a t -> 'a t

module API : sig
  type expr
  val workflow : ?target:path -> cmd list -> 'a t
  val program :
    ?path:package workflow list ->
    string ->
    ?stdin:expr -> ?stdout:expr -> ?stderr:expr ->
    expr list -> cmd

  val target : unit -> expr
  val string : string -> expr
  val int : int -> expr
  val float : float -> expr
  val path : path -> expr
  val dep : _ t -> expr
  val option : ('a -> expr) -> 'a option -> expr
  val list : ('a -> expr) -> ?sep:string -> 'a list -> expr
  val enum : ('a * string) list -> 'a -> expr
  val opt : string -> ('a -> expr) -> 'a -> expr

  val mkdir : expr -> cmd
  val mkdir_p : expr -> cmd
end

(* let body = program "bowtie" [ *)
(*     opt int "-v" 1 ; *)
(*     deps fqs ; *)
(*   ] *)

(* let r = rule [ "aligned_reads" ; sample_id ] body *)

