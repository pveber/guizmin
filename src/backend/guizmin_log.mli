type level = [ `debug | `info | `warning | `error ]

type event = [
| `started_build of Guizmin.Workflow.u
| `finished_build of Guizmin.Workflow.u
| `failed_build of Guizmin.Workflow.u * string option
| `msg of level * string
]

type timestamp = Core.Std.Time.t

module Entry : sig
  type t = event * timestamp
  val to_string : t -> string
end

type t

val make : ?db:Guizmin_db.t -> ?hook:(Entry.t -> unit) -> unit -> t

val started_build : t -> Guizmin.Workflow.u -> unit
val finished_build : t -> Guizmin.Workflow.u -> unit
val failed_build : t -> ?msg:string -> Guizmin.Workflow.u -> unit

val debug : t -> ('a,unit,string,unit) format4 -> 'a
val info : t -> ('a,unit,string,unit) format4 -> 'a
val warning : t -> ('a,unit,string,unit) format4 -> 'a
val error : t -> ('a,unit,string,unit) format4 -> 'a
