open Core.Std

type verbosity = Normal | Quiet | Verbose

type copts = {
  debug : bool ;
  verbosity : verbosity
}

type dopts = {
  backend : backend ;
  np : int ;
  max_np_per_job : int ;
  mem : int
}
and backend = Local | Pbs

let mkdir_p path =
  Sys.command_exn ("mkdir -p " ^ path)

let symlink src dst =
  let create_link =
    if Sys.file_exists dst = `Yes then Unix.(
        if (lstat dst).st_kind <> S_LNK || readlink dst <> src
        then (
          unlink dst ;
          true
        )
        else false
      )
    else true
  in
  if create_link
  then
    Sys.command_exn (Printf.sprintf "ln -s `readlink -f %s` %s" src dst)

let file_is_empty path =
  Unix.((stat path).st_size = 0L)

module Lwt_infix = struct
  let ( >>= ) = Lwt.bind

  let ( >|= ) x f = Lwt.map f x

  let ( >>? ) x f =
    Lwt.bind x (function
        | None -> Lwt.return None
        | Some x -> f x
      )

  let ( >|? ) x f = Lwt.map (Option.map ~f) x
end

let lwt_option_map x ~f =
  match x with
  | None -> Lwt.return None
  | Some x -> Lwt.bind (f x) (fun y -> Lwt.return (Some y))

let lwt_option_bind x ~f =
  match x with
  | None -> Lwt.return None
  | Some x -> Lwt.bind (f x) (fun y -> Lwt.return y)
