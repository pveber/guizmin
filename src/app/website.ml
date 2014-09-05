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

  val raw : ?path:path -> 'a Bistro_workflow.t -> filename page

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
    | Raw of Bistro_workflow.u


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

  let a d elts =
    Html5.M.(a ~a:[a_href (string_of_path (path d))] elts)

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

  let raw ?path w =
    let u = Bistro_workflow.u w in
    let path = match path with
      | Some p -> p
      | None -> [ "files" ; Bistro_workflow.digest u ] in
    let page = { path ; kind = Raw u } in
    add_page page path ;
    page

  let check_missing_generator () =
    match missing_generator () with
    | Some path ->
      let msg = sprintf "Guizmin.Website: no generator for page %s" (string_of_path path) in
      failwith msg
    | None -> ()

  let fs_path output_dir x =
    Filename.concat output_dir (string_of_path (path x))

  let generate_html_page fs_path page f =
    f () >>= fun html ->
    let fs_path = fs_path page in
    mkdir_p (Filename.dirname fs_path) ;
    Core.Out_channel.with_file fs_path ~f:(fun oc ->
        Html5.P.print ~output:(output_string oc) html
      ) ;
    Lwt.return ()

  let generate_raw_page fs_path workflow_output page u =
    let fs_path = fs_path page in
    mkdir_p (Filename.dirname fs_path) ;
    workflow_output u >>= fun cache_path ->
    symlink cache_path fs_path ;
    Lwt.return ()

  let generate_page fs_path workflow_output page = match page.kind with
    | Html_page ->
      let f = Hashtbl.find generators page.path in
      generate_html_page fs_path page f
    | Raw u ->
      generate_raw_page fs_path workflow_output page u

  let generate ~workflow_output ~output_dir =
    let fs_path = fs_path output_dir in
    check_missing_generator () ;
    Hashtbl.fold
      (fun _ page accu -> generate_page fs_path workflow_output page :: accu)
      pages []
    |> Lwt.join

end
