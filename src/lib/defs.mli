type path = string list

val string_of_path : path -> string
val path_of_string : string -> path

val digest : 'a -> string

val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
