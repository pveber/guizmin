(* type path = string list
 * 
 * val string_of_path : path -> string
 * val path_of_string : string -> path
 * 
 * val digest : 'a -> string
 * 
 * val python_version : [ `M_m ] -> string option *)

module Infix : sig
  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

(*
   remove duplicates *and* keep original order
   not tail-recursive and quadratic complexity
*)
val unique : 'a list -> 'a list
