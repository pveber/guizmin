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
  version : int option ; (** Version number of the wrapper *)
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
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file
  type bash_script = ([`bash_script], [`text]) file

  class type ['a] tabular = object ('a)
    constraint 'a = < columns : 'b ; header : ([< `yes | `no] as 'c) ;
                      sep : 'd ; comment : 'e ; .. >
    inherit [[`tabular], [`text]] file
    method columns : 'b
    method header : 'c
    method sep : 'd
    method comment : 'e
  end

  class type ['a] tsv = object
    inherit [ < sep : [`tab] ; .. > as 'a ] tabular
  end

end

open Types

val step :
  ?np:int -> ?mem:int -> ?timeout:int ->
  ?version:int ->
  cmd list -> 'a t
val extract : _ directory t -> path -> 'a t
val input : string -> 'a t


module API : sig
  type shell_expr
  val workflow :
    ?np:int -> ?mem:int -> ?timeout:int ->
    ?version:int ->
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

  val dest : shell_expr
  val tmp : shell_expr
  val string : string -> shell_expr
  val int : int -> shell_expr
  val float : float -> shell_expr
  val path : path -> shell_expr
  val dep : _ t -> shell_expr
  val option : ('a -> shell_expr) -> 'a option -> shell_expr
  val list : ('a -> shell_expr) -> ?sep:string -> 'a list -> shell_expr
  val seq : ?sep:string -> shell_expr list -> shell_expr
  val enum : ('a * string) list -> 'a -> shell_expr
  val opt : string -> ('a -> shell_expr) -> 'a -> shell_expr
  val opt' : string -> ('a -> shell_expr) -> 'a -> shell_expr
  val flag : ('a -> shell_expr) -> 'a -> bool -> shell_expr

  val ( // ) : shell_expr -> string -> shell_expr

  val or_list : cmd list -> cmd
  val and_list : cmd list -> cmd
  val pipe : cmd list -> cmd

  val with_env : (string * shell_expr) list -> cmd -> cmd

  val mkdir : shell_expr -> cmd
  val mkdir_p : shell_expr -> cmd
  val wget : string -> ?dest:shell_expr -> unit -> cmd
  val cd : shell_expr -> cmd
  val rm_rf : shell_expr -> cmd
  val mv : shell_expr -> shell_expr -> cmd
end

val deps : u -> u list
val shell_script : (u -> path) -> build_target:path -> tmp_target:path -> cmd list -> string list


(* let body = program "bowtie" [ *)
(*     opt int "-v" 1 ; *)
(*     deps fqs ; *)
(*   ] *)

(* let r = rule [ "aligned_reads" ; sample_id ] body *)

