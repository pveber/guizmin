open Core.Std
open Guizmin.Defs

let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )


(* FIXME: this implementation leaks memory. When calling run on a
   [d] with new workflows, the size of [d.threads] grows without any
   possibility to shrink. A good fix should be to switch to a weak
   array to store threads. *)
type t = {
  mutable threads : unit Lwt.t String.Map.t ;
  mutable on : bool ;
  db : Guizmin_db.t ;
  log : Guizmin_log.t ;
  backend : Guizmin_lwt_backend.t ;
}

let make db log backend = {
  threads = String.Map.empty ;
  on = true ;
  db ; log ; backend
}


let remove_if_exists fn =
  if Sys.file_exists fn = `Yes then
    Lwt_process.exec ("", [| "rm" ; "-r" ; fn |]) >|= ignore
  else
    Lwt.return ()

let thread_of_rule blog (backend : Guizmin_lwt_backend.t) db r dep_threads =
  let open Guizmin.Workflow in
  match r with { np ; mem ; timeout ; script } ->
  let x = Step r in
  Lwt.join dep_threads >>= fun () ->
  (
    let stdout = Guizmin_db.stdout_path db x in
    let stderr = Guizmin_db.stderr_path db x in
    let dest = Guizmin_db.build_path db x in
    let tmp = Guizmin_db.tmp_path db x in
    let build_target = path_of_string dest in
    let tmp_target = path_of_string tmp in
    let script =
      shell_script (Guizmin_db.path db % path_of_string) ~build_target ~tmp_target script
      |> String.concat ~sep:" && "
    in
    remove_if_exists stdout >>= fun () ->
    remove_if_exists stderr >>= fun () ->
    remove_if_exists dest >>= fun () ->
    remove_if_exists tmp >>= fun () ->
    Lwt_unix.mkdir tmp 0o750 >>= fun () ->
    Guizmin_log.started_build blog x ;
    backend ~np ~mem ~timeout ~stdout ~stderr script >>= fun backend_response ->
    match backend_response, Sys.file_exists_exn dest with
    | `Ok, true ->
      Guizmin_log.finished_build blog x ;
      remove_if_exists tmp >>= fun () ->
      Guizmin_db.created db x ;
      Lwt_unix.rename dest (Guizmin_db.path db x)
    | `Ok, false ->
      let msg = sprintf "Build of workflow %s failed: rule failed to produce its target at the prescribed location" (digest x) in
      Guizmin_log.failed_build ~msg blog x ;
      Lwt.fail (Failure msg)
    | `Error, _ ->
      Guizmin_log.failed_build blog x ;
      Lwt.fail (Failure (sprintf "Build of workflow %s failed!" (digest x)))
  )

let rec thread_of_workflow f db map w =
  let open Guizmin.Workflow in
  let id = digest w in
  match Map.find map id with
  | Some t -> (t, map)
  | None -> (
      let open Guizmin.Workflow in
      match w with
      | Input p ->
        Lwt.wrap (fun () ->
            let p = string_of_path p in
            if Sys.file_exists p <> `Yes
            then failwithf "File %s is declared as an input of a workflow but does not exist." p ()
          ),
        map
      | Extract (dir,p) as x ->
        let p = string_of_path p in
        let dir_path = Guizmin_db.path db dir in
        let check_in_dir () =
          if Sys.file_exists (Guizmin_db.path db x) <> `Yes
          then (
            let msg = sprintf "No file or directory named %s in directory workflow." p in
            Lwt.fail (Failure msg)
          )
          else Lwt.return ()
        in
        if Sys.file_exists_exn dir_path then (
          Guizmin_db.used db dir ;
          check_in_dir (), map
        )
        else (
          let dir_thread, map = thread_of_workflow f db map dir in
          dir_thread >>= check_in_dir,
          map
        )
      | Step r as x ->
        let dest_path = Guizmin_db.path db x in
        if Sys.file_exists_exn dest_path then (
          Guizmin_db.used db x ;
          Lwt.return (), map
        )
        else (
          let dep_threads, map =
            List.fold_right (Guizmin.Workflow.deps w) ~init:([], map) ~f:(fun w (accu, map) ->
                let t, map = thread_of_workflow f db map w in
                t :: accu, map
              )
          in
          let t = f db r dep_threads in
          t, String.Map.add map ~key:id ~data:t
        )
    )

let send' d u =
  if d.on then (
    let t, threads = thread_of_workflow (thread_of_rule d.log d.backend) d.db d.threads u in
    d.threads <- threads ;
    Some t
  )
  else None

let send d w = send' d Guizmin.Workflow.(w : _ t :> u)

let shutdown d =
  d.on <- false ;
  String.Map.to_alist d.threads
  |> List.map ~f:snd
  |> Lwt.join
