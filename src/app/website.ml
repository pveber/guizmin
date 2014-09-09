open Printf
open Common

type path = string list
type filename = string
type html_elt = [`Html] Html5.M.elt

let string_of_path l = String.concat "/" l

module type S = sig
  type 'a page

  val path : _ page -> path

  val href : _ page -> string

  val a : _ page -> 'a Html5.M.elt list -> [`A of 'a] Html5.M.elt

  val html_page :
    path ->
    ?f:(unit -> html_elt Lwt.t) ->
    unit -> html_elt page

  val raw : ?path:path -> ?extract:bool -> 'a Bistro_workflow.t -> filename page

  val register : html_elt page -> (unit -> html_elt Lwt.t) -> unit

  val generate :
    workflow_output:(Bistro_workflow.u -> string Lwt.t) ->
    output_dir:string ->
    unit Lwt.t
end

module Make(X : sig end) = struct
  type 'a page = {
    path : path ;
    kind : kind
  }
  and kind =
    | Html_page
    | Raw of Bistro_workflow.u
    | Embedded_raw of Bistro_workflow.u * raw_embedding
  and raw_embedding = {
    container : Bistro_workflow.u ;
    container_path : path
  }

  let pages = Hashtbl.create 253

  let generators = Hashtbl.create 253

  let missing_generator () =
    let module M = struct exception Found of path end in
    let aux k v () =
      if v.kind = Html_page && not (Hashtbl.mem generators k)
      then raise (M.Found k)
    in
    try Hashtbl.fold aux pages () ; None
    with M.Found p -> Some p

  let path p = p.path

  let files_path u =
    [ "files" ; Bistro_workflow.digest u ]

  let rec unselect = function
    | Bistro_workflow.Input _ | Bistro_workflow.Rule _ as u -> u, []
    | Bistro_workflow.Select (u, q) ->
      let r, p = unselect u in
      r, p @ [ q ]

  let href d = string_of_path (path d)

  let a d elts =
    Html5.M.(a ~a:[a_href (href d)] elts)

  let add_page page path =
    if Hashtbl.mem pages path then (
      let path = string_of_path path in
      let msg = sprintf "Guizmin.Website: attempt to create page %s twice" path in
      failwith msg
    ) ;
    Hashtbl.add pages path page

  let register page f =
    Hashtbl.add generators page.path f

  let html_page p ?f () =
    let page = { path = p ; kind = Html_page } in
    add_page page p ;
    (match f with
     | Some f -> register page f
     | None -> ()) ;
    page

  let raw ?path ?(extract = false) w =
    let u = Bistro_workflow.u w in
    let path, kind = match path, u, extract with
      | Some p, Bistro_workflow.Select _, true
      | Some p, Bistro_workflow.Input _, _
      | Some p, Bistro_workflow.Rule _, _ ->
        p, Raw u
      | None, Bistro_workflow.Select _, true
      | None, Bistro_workflow.Input _, _
      | None, Bistro_workflow.Rule _, _ ->
        files_path u, Raw u
      | Some p, Bistro_workflow.Select _, false ->
        let u', paths_in_u' = unselect u in
        let path_in_u' = Core_kernel.Core_list.reduce_exn paths_in_u' ~f:Filename.concat in (* paths cannot be empty since u is a select *)
        p @ [ path_in_u' ], Embedded_raw (u, { container = u' ; container_path = p })
      | None, Bistro_workflow.Select _, false ->
        let u', paths_in_u' = unselect u in
        let path_in_u' = Core_kernel.Core_list.reduce_exn paths_in_u' ~f:Filename.concat in (* paths cannot be empty since u is a select *)
        let p = files_path u' in
        p @ [ path_in_u' ], Embedded_raw (u, { container = u' ; container_path = p })
    in
    let page = { path ; kind } in
    add_page page path ;
    page

  let check_missing_generator () =
    match missing_generator () with
    | Some path ->
      let msg = sprintf "Guizmin.Website: no generator for page %s" (string_of_path path) in
      failwith msg
    | None -> ()

  let fspath output_dir x =
    Filename.concat output_dir (string_of_path x)

  let generate_html_page fspath page f =
    f () >>= fun html ->
    let fspath = fspath (path page) in
    mkdir_p (Filename.dirname fspath) ;
    Core.Out_channel.with_file fspath ~f:(fun oc ->
        Html5.P.print ~output:(output_string oc) html
      ) ;
    Lwt.return ()

  let alias fspath p1 p2 =
    printf "alias %s %s\n" (string_of_path p1) (string_of_path p2) ;
    let p2_to_p1 = List.map (fun _ -> "..") (List.tl p2) @ p1 in
    let dst = fspath p2 in
    mkdir_p (Filename.dirname dst) ;
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

(*  let generate_raw_page fspath workflow_output path u =
    let files_fspath = fspath (files_path u) in
    mkdir_p (Filename.dirname fspath) ;
    workflow_output u >>= fun cache_fspath ->
    symlink cache_fspath fspath ;
    Lwt.return ()
*)
  let generate_raw_page fspath workflow_output path u =
    let u', path_in_u' = unselect u in
    let files_fspath = fspath (files_path u') in
    mkdir_p (Filename.dirname files_fspath) ;
    workflow_output u' >>= fun cache_fspath ->
    symlink cache_fspath files_fspath ;
    if path <> files_path u' then alias fspath (files_path u') path ;
    Lwt.return ()

  let generate_page fs_path workflow_output page = match page.kind with
    | Html_page ->
      let f = Hashtbl.find generators page.path in
      generate_html_page fs_path page f
    | Raw u ->
      generate_raw_page fs_path workflow_output page.path u
    | Embedded_raw (u, e) -> (* FIXME: there is a race condition here if the container is generated several times !!! *)
      print_endline (string_of_path page.path) ;
      generate_raw_page fs_path workflow_output e.container_path e.container

  let generate ~workflow_output ~output_dir =
    let fspath = fspath output_dir in
    check_missing_generator () ;
    Hashtbl.fold
      (fun _ page accu -> generate_page fspath workflow_output page :: accu)
      pages []
    |> Lwt.join

end
