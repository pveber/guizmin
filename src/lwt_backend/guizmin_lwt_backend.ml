open Core.Std
open Printf

type t =
  np:int -> mem:int -> timeout:int ->
  stdout:string -> stderr:string ->
  string -> [`Ok | `Error] Lwt.t


let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let string_of_process_status ps =
  Unix.Exit_or_signal.(
    of_unix ps
    |> to_string_hum
  )

let redirection filename =
  Lwt_unix.openfile filename Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) 0o640 >>= fun fd ->
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let local_aux (log : Guizmin_log.t) ~np ~mem ~timeout ~stdout ~stderr script =
  let script_file = Filename.temp_file "guizmin" ".sh" in
  Guizmin_log.debug log "Exec script %s:\n%s\n" script_file script ;
  Lwt_io.(with_file ~mode:output script_file (fun oc -> write oc script)) >>= fun () ->
  begin
    redirection stdout >>= fun stdout ->
    redirection stderr >>= fun stderr ->
    let cmd = ("", [| "sh" ; script_file |]) in
    Lwt_process.exec ~stdout ~stderr cmd
  end >>= fun exitcode ->
  match exitcode with
  | Caml.Unix.WEXITED 0 ->
    Lwt_unix.unlink script_file >>= fun () ->
    Lwt.return `Ok
  | _ -> (
    Guizmin_log.error log
      "Script %s failed!\nError status: %s\nstdout: %s\nstderr: %s\n"
      script_file (string_of_process_status exitcode) stdout stderr ;
    Lwt.return `Error
  )

let local ~np ~mem log : t =
  let pool = Guizmin_pool.create ~np ~mem in
  fun ~np ~mem ~timeout ~stdout ~stderr script ->
    Guizmin_pool.use pool ~np ~mem ~f:(fun ~np ~mem ->
      local_aux log ~np ~mem ~stdout ~stderr ~timeout script
    )
