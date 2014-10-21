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

val string_of_cmd : path -> cmd -> string

val make : ?target:path -> cmd list -> 'a t
val in_target : _ t -> path -> 'a t
val input : path -> 'a t

module Sh : sig
  type expr
  val program : string -> ?stdout:expr -> ?stderr:expr -> expr list -> cmd
  val target : expr
  val string : string -> expr
  val int : int -> expr
  val float : float -> expr
  val path : path -> expr
  val dep : _ t -> expr
  val option : ('a -> expr) -> 'a option -> expr
  val list : ('a -> expr) -> ?sep:string -> 'a list -> expr
  val enum : ('a * string) list -> 'a -> expr
  val opt : ('a -> expr) -> string -> 'a -> expr
end

(* let body = program "bowtie" [ *)
(*     opt int "-v" 1 ; *)
(*     deps fqs ; *)
(*   ] *)

(* let r = rule [ "aligned_reads" ; sample_id ] body *)

