open Printf
open Common
open Common.Lwt_infix

type path = string list
type filename = string
type html_elt = [`Html] Html5.M.elt

let string_of_path = Guizmin.Defs.string_of_path

module type S = sig
  type 'a page

  val path : _ page -> path

  val href : _ page -> string

  val a : _ page -> 'a Html5.M.elt list -> [> `A of 'a] Html5.M.elt

  val html_page :
    path ->
    ?f:(unit -> html_elt Lwt.t) ->
    unit -> html_elt page

  val file_page : ?path:path -> ?in_situ:bool -> _ Guizmin.Workflow.t -> Guizmin.Workflow.u page

  val register : html_elt page -> (unit -> html_elt Lwt.t) -> unit

  val generate :
    workflow_output:(Guizmin.Workflow.u -> filename Lwt.t) ->
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
    | File_page of Guizmin.Workflow.u
    | In_situ_file_page

  let pages = Hashtbl.create 253

  let add_page page path =
    if Hashtbl.mem pages path then (
      let path = string_of_path path in
      let msg = sprintf "Guizmin.Website: attempt to create page %s twice" path in
      failwith msg
    ) ;
    Hashtbl.add pages path page

  let rec prefix_diff ~prefix p = match prefix, p with
    | [], p -> Some p
    | _, [] -> None
    | h :: t, h' :: t' ->
      if h = h' then prefix_diff ~prefix:t t' else None

  (* Find file pages whose workflow contains the workflow Extract (u, p) *)
  let find_strict_containers u p =
    if p = [] then []
    else (
      Hashtbl.fold
        (fun _ page accu ->
           match page.kind with
           | File_page (Guizmin.Workflow.Extract (v, q)) when u = v ->
             (
               match prefix_diff ~prefix:q p with
               | None
               | Some [] -> accu
               | Some r -> (page, r) :: accu
             )
           | File_page v ->
             if u = v then (page, []) :: accu
             else accu
           | _ -> accu)
        pages []
    )

  let find_largest_strict_container u p =
    Core.Core_list.reduce
      (find_strict_containers u p)
      ~f:(fun ((_,p) as x) ((_,q) as y) ->
          if List.length p < List.length q then x else y)

  let generators = Hashtbl.create 253

  let register page f =
    Hashtbl.add generators page.path f

  let missing_generator () =
    let module M = struct exception Found of path end in
    let aux k v () =
      if v.kind = Html_page && not (Hashtbl.mem generators k)
      then raise (M.Found k)
    in
    try Hashtbl.fold aux pages () ; None
    with M.Found p -> Some p


  let html_page p ?f () =
    let page = { path = p ; kind = Html_page } in
    add_page page p ;
    (match f with
     | Some f -> register page f
     | None -> ()) ;
    page

  let file_path u =
    [ "file" ; Guizmin.Defs.digest u ]

  let file_page ?path ?(in_situ = true) w =
    let u = Guizmin.Workflow.(w : _ t :> u) in
    let path = match path with
      | Some p -> p
      | None -> file_path u
    in
    let page, path =
      match u, in_situ with
      | Guizmin.Workflow.Extract (v, p), true ->
        let occupied =
          match Hashtbl.find pages path with
          | { kind = File_page w } -> v <> w
          | _ -> true
          | exception Not_found ->
            let page = { path ; kind = File_page v } in
            add_page page path ;
            false
        in
        if occupied then (
          let msg =
            sprintf
              "Cannot create page %s because %s is already occupied"
              (string_of_path (path @ p))
              (string_of_path path)
          in
          failwith msg
        )
        else (
          let path = path @ p in
          let page = { path ; kind = In_situ_file_page } in
          page, path
        )
      | _ ->
        { path ; kind = File_page u }, path
    in
    add_page page path ;
    page

  let path p = p.path


  let href d = string_of_path (path d)

  let a d elts =
    Html5.M.(a ~a:[a_href (href d)] elts)



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
    mkdir_p (Filename.dirname dst) ;
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

  let generate_file_page fspath workflow_output path u =
    let file_fspath = fspath (file_path u) in
    mkdir_p (Filename.dirname file_fspath) ;
    workflow_output u >>= fun cache_fspath ->
    symlink cache_fspath file_fspath ;
    if path <> file_path u then alias fspath (file_path u) path ;
    Lwt.return ()

  let classify_page page = match page.kind with
    | Html_page -> `Html_page
    | File_page (Guizmin.Workflow.Extract (v, p) as u) -> (
        match find_largest_strict_container v p with
        | None -> `Root_file u
        | Some (root, q) -> `Leaf_file (root, q)
      )
    | File_page u -> `Root_file u
    | In_situ_file_page -> `In_situ_file_page


  let generate ~workflow_output ~output_dir =
    let fspath = fspath output_dir in
    check_missing_generator () ;
    Hashtbl.fold
      (fun _ page accu ->
         let t =
           match classify_page page with
           | `Html_page ->
             let f = Hashtbl.find generators page.path in
             generate_html_page fspath page f
           | `Root_file u ->
             generate_file_page fspath workflow_output page.path u
           | `Leaf_file (root, inside_path) ->
             alias fspath (root.path @ inside_path) page.path ;
             Lwt.return ()
           | `In_situ_file_page ->
             Lwt.return ()
         in
         t :: accu)
      pages []
    |> Lwt.join

end
