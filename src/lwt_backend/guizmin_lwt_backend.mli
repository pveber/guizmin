type t =
  np:int -> mem:int -> timeout:int ->
  stdout:string -> stderr:string ->
  string -> [`Ok | `Error] Lwt.t

val local : np:int -> mem:int -> Guizmin_log.t -> t
