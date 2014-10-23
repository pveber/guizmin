type t
val make : Guizmin_db.t -> Guizmin_log.t -> Guizmin_lwt_backend.t -> t
val send : t -> _ Guizmin.Workflow.t -> unit Lwt.t option
val send' : t -> Guizmin.Workflow.u -> unit Lwt.t option
val shutdown : t -> unit Lwt.t
