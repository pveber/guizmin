open Core.Std

type verbosity = Normal | Quiet | Verbose

type copts = {
  debug : bool ;
  verbosity : verbosity
}

let mkdir_p path =
  Sys.command_exn ("mkdir -p " ^ path)
