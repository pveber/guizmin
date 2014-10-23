open Core.Std
open Guizmin.Defs

type t = string

let cache_dir base = Filename.concat base "cache"
let build_dir base = Filename.concat base "build"
let tmp_dir base = Filename.concat base "tmp"
let stderr_dir base = Filename.concat base "stderr"
let stdout_dir base = Filename.concat base "stdout"
let log_dir base = Filename.concat base "logs"
let history_dir base = Filename.concat base "history"

let well_formed_db path =
  if Sys.file_exists_exn path then (
    Sys.file_exists_exn (cache_dir path)
    && Sys.file_exists_exn (build_dir path)
    && Sys.file_exists_exn (tmp_dir path)
    && Sys.file_exists_exn (stderr_dir path)
    && Sys.file_exists_exn (stdout_dir path)
    && Sys.file_exists_exn (log_dir path)
    && Sys.file_exists_exn (history_dir path)
  )
  else false

let init base =
  if Sys.file_exists_exn base
  then (
    if not (well_formed_db base)
    then invalid_argf "Guizmin_db.init: the path %s is not available for a guizmin database" base ()
  )
  else (
    Unix.mkdir_p (tmp_dir base) ;
    Unix.mkdir_p (build_dir base) ;
    Unix.mkdir_p (cache_dir base) ;
    Unix.mkdir_p (stderr_dir base) ;
    Unix.mkdir_p (stdout_dir base) ;
    Unix.mkdir_p (log_dir base) ;
    Unix.mkdir_p (history_dir base)
  ) ;
  base

let aux_path f db w =
  Filename.concat (f db) (digest w)

let log_path db w = aux_path log_dir db w
let build_path db w = aux_path build_dir db w
let tmp_path db w = aux_path tmp_dir db w
let stdout_path db w = aux_path stdout_dir db w
let stderr_path db w = aux_path stderr_dir db w
let history_path db w = aux_path history_dir db w

let rec path db = Guizmin.Workflow.(function
    | Input p -> string_of_path p
    | Extract (dir, p) ->
      Filename.concat (path db dir) (string_of_path p)
    | Step r as w -> aux_path cache_dir db w
  )



let used_tag = "U"
let created_tag = "C"

let history_tag_of_string x =
  if x = used_tag then `used
  else if x = created_tag then `created
  else invalid_argf "Guizmin_db.history_tag_of_string: %s" x ()

let append_history ~db ~msg u =
  Out_channel.with_file ~append:true (history_path db u) ~f:(fun oc ->
      let time_stamp = Time.to_string_fix_proto `Local (Time.now ()) in
      fprintf oc "%s: %s\n" time_stamp msg
    )

let rec used db = Guizmin.Workflow.(function
  | Input _ -> ()
  | Step _ as u -> append_history ~db ~msg:used_tag u
  | Extract (u, _) -> used db u
  )

let created db = Guizmin.Workflow.(function
  | Input _
  | Extract _ -> raise (Invalid_argument "")
  | Step _ as u -> append_history ~db ~msg:created_tag u
  )

let parse_history_line l =
  let stamp, tag = String.lsplit2_exn l ~on:':' in
  Time.of_string_fix_proto `Local stamp,
  history_tag_of_string (String.lstrip tag)

let history db = Guizmin.Workflow.(function
  | Input _ | Extract _ -> invalid_argf "Guizmin_db.history: this workflow cannot have an history" ()
  | Step _ as u ->
    let p_u = path db u in
    if Sys.file_exists_exn p_u then
      List.map (In_channel.read_lines p_u) ~f:parse_history_line
    else
      []
  )
let echo ~path msg =
  Out_channel.with_file ~append:true path ~f:(fun oc ->
      output_string oc msg ;
      output_string oc "\n"
    )

let log db fmt =
  let f msg =
    let path =
      Filename.concat
        (log_dir db)
        (Time.format (Time.now ()) "%Y-%m-%d.log")
    in
    echo ~path msg
  in
  Printf.ksprintf f fmt
