open Printf
open Common

type path = string list
type filename = string
type html_elt = [`Html] Html5.M.elt

let string_of_path l = String.concat "/" l

module type S = sig
  type 'a page

  val path : _ page -> path

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
    path : string list ;
    kind : kind
  }
  and kind =
    | Html_page
    | Raw of Bistro_workflow.u * [`extracted | `not_extracted]


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

  let a d elts =
    let href = match d with
      | { kind = Html_page }
      | { kind = Raw (_, `extracted) }
      | { kind = Raw (Bistro_workflow.Rule _, `not_extracted) }
      | { kind = Raw (Bistro_workflow.Input _, `not_extracted) } ->
        string_of_path (path d)
      | { kind = Raw (Bistro_workflow.Select (_,_) as u, `not_extracted) } ->
        let u', paths = unselect u in
        let path_in_u' = Core_kernel.Core_list.reduce_exn paths ~f:Filename.concat in (* paths cannot be empty since u is a select *)
        Filename.concat (string_of_path (path d)) path_in_u'
    in
    Html5.M.(a ~a:[a_href href] elts)

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
    let path = match path with
      | Some p -> p
      | None -> files_path u in
    let page = { path ; kind = Raw (u, if extract then `extracted else `not_extracted) } in
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
    let p2_to_p1 = List.map (fun _ -> "..") (List.tl p2) @ p1 in
    let dst = fspath p2 in
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

  let generate_raw_page fspath workflow_output page u extract =
    if extract then (
      let fspath = fspath page.path in
      mkdir_p (Filename.dirname fspath) ;
      workflow_output u >>= fun cache_fspath ->
      symlink cache_fspath fspath ;
      Lwt.return ()
    )
    else (
      let u', path_in_u' = unselect u in
      let files_fspath = fspath (files_path u') in
      mkdir_p (Filename.dirname files_fspath) ;
      workflow_output u' >>= fun cache_fspath ->
      symlink cache_fspath files_fspath ;
      alias fspath (files_path u') page.path ;
      Lwt.return ()
    )

  let generate_page fs_path workflow_output page = match page.kind with
    | Html_page ->
      let f = Hashtbl.find generators page.path in
      generate_html_page fs_path page f
    | Raw (u, extract) ->
      generate_raw_page fs_path workflow_output page u (extract = `extracted)

  let generate ~workflow_output ~output_dir =
    let fspath = fspath output_dir in
    check_missing_generator () ;
    Hashtbl.fold
      (fun _ page accu -> generate_page fspath workflow_output page :: accu)
      pages []
    |> Lwt.join

end
