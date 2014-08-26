open Core.Std

type verbosity = Normal | Quiet | Verbose

type copts = {
  debug : bool ;
  verbosity : verbosity
}

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

let ( >>= ) = Lwt.bind
let ( >|= ) x f = Lwt.map f x
