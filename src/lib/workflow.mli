open Defs

type 'a t = private u
and u =
  | Input of path
  | Extract of u * path
  | Step of step
and step = {
  deps : u list ;
  script : cmd list ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int ; (** Maximum allowed running time in hours *)
}
and cmd

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
  type bash_script = ([`bash_script], [`text]) file

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

val step :
  ?np:int -> ?mem:int -> ?timeout:int ->
  cmd list -> 'a t
val extract : _ directory t -> path -> 'a t
val input : string -> 'a t


module API : sig
  type shell_expr
  val workflow :
    ?np:int -> ?mem:int -> ?timeout:int ->
    cmd list -> 'a t

  val program :
    ?path:package workflow list ->
    ?pythonpath:package workflow list ->
    string ->
    ?stdin:shell_expr -> ?stdout:shell_expr -> ?stderr:shell_expr ->
    shell_expr list -> cmd

  val bash :
    ?path:package workflow list ->
    bash_script workflow ->
    ?stdin:shell_expr -> ?stdout:shell_expr -> ?stderr:shell_expr ->
    shell_expr list -> cmd

  val target : unit -> shell_expr
  val string : string -> shell_expr
  val int : int -> shell_expr
  val float : float -> shell_expr
  val path : path -> shell_expr
  val dep : _ t -> shell_expr
  val option : ('a -> shell_expr) -> 'a option -> shell_expr
  val list : ('a -> shell_expr) -> ?sep:string -> 'a list -> shell_expr
  val seq : shell_expr list -> shell_expr
  val enum : ('a * string) list -> 'a -> shell_expr
  val opt : string -> ('a -> shell_expr) -> 'a -> shell_expr
  val flag : ('a -> shell_expr) -> 'a -> bool -> shell_expr

  val ( // ) : shell_expr -> string -> shell_expr

  val mkdir : shell_expr -> cmd
  val mkdir_p : shell_expr -> cmd
  val wget : string -> cmd
  val cd : shell_expr -> cmd
  val rm_rf : shell_expr -> cmd
  val mv : shell_expr -> shell_expr -> cmd
end

val deps : u -> u list
val shell_script : (u -> path) -> path -> cmd list -> string list


(* let body = program "bowtie" [ *)
(*     opt int "-v" 1 ; *)
(*     deps fqs ; *)
(*   ] *)

(* let r = rule [ "aligned_reads" ; sample_id ] body *)

