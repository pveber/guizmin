open Core.Std
open Printf

type path = string list
type html_elt = [`Html] Html5.M.elt

let ( >>= ) = Lwt.bind

let ( >|= ) x f = Lwt.map f x

let ( >>? ) x f =
  Lwt.bind x (function
      | None -> Lwt.return None
      | Some x -> f x
    )

let ( >|? ) x f = Lwt.map (Option.map ~f) x

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

module type Engine = sig
  val build : Bistro.Workflow.u -> string Lwt.t
end

let string_of_path = Misc.string_of_path

module Make(E : Engine) = struct
  type page = {
    path : path ;
    kind : kind
  }
  and kind =
    | Html_page of (unit -> html_elt Lwt.t)
    | File_page of Bistro.Workflow.u
    | In_situ_file_page of Bistro.Workflow.u * path * path (* path of the container, path inside the container *)

  let inserted_page page =
    match page.kind with
    | In_situ_file_page (u, path_u, _) ->
      { path = path_u ; kind = File_page u }
    | Html_page _ | File_page _ -> page

  let validate_addition site page =
    let ins_page = inserted_page page in
    if List.mem site ins_page then `already_in
    else
      let conflict page' =
        page.path = page'.path && page.kind <> page'.kind
      in
      if List.exists site ~f:conflict
      then `conflict ins_page.path
      else `insert ins_page


  let add_page site page =
    match validate_addition site page with
    | `already_in -> site
    | `conflict p -> failwithf "Path %s is already taken" (string_of_path p) ()
    | `insert page -> page :: site

  let site = ref []

  let html_page p f =
    let page = { path = p ; kind = Html_page f } in
    site := add_page !site page ;
    page

  let file_path u =
    [ "file" ; Misc.digest u ]

  let file_page ?path ?(in_situ = true) w =
    let u = Bistro.Workflow.((w : _ t :> u)) in
    let path = match path with
      | Some p -> p
      | None -> file_path u
    in
    let page = match u, in_situ with
      | Bistro.Workflow.Extract (_, v, p), true ->
        { path = path @ p ;
          kind = In_situ_file_page (v, path, p) }
      | _ -> { path ; kind = File_page u }
    in
    site := add_page !site page ;
    page


  let path p = p.path


  let href d = string_of_path (path d)

  let a d elts =
    Html5.M.(a ~a:[a_href (href d)] elts)


  (* === REPO GENERATION === *)
  let fspath output_dir x =
    Filename.concat output_dir (string_of_path x)

  let generate_html_page fspath page f =
    f () >>= fun html ->
    let fspath = fspath (path page) in
    Unix.mkdir_p (Filename.dirname fspath) ;
    Core.Out_channel.with_file fspath ~f:(fun oc ->
        Html5.P.print ~output:(output_string oc) html
      ) ;
    Lwt.return ()

  let alias fspath p1 p2 =
    let p2_to_p1 = List.map ~f:(fun _ -> "..") (List.tl_exn p2) @ p1 in
    let dst = fspath p2 in
    Unix.mkdir_p (Filename.dirname dst) ;
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

  let generate_file_page fspath path_u u =
    let file_fspath = fspath (file_path u) in
    Unix.mkdir_p (Filename.dirname file_fspath) ;
    E.build u >>= fun cache_fspath ->
    symlink cache_fspath file_fspath ;
    if path_u <> file_path u then alias fspath (file_path u) path_u ;
    Lwt.return ()


  let generate dir =
    let fspath = fspath dir in
    List.map !site ~f:(fun page ->
        match page.kind with
        | Html_page f ->
          generate_html_page fspath page f
        | File_page u ->
          generate_file_page fspath page.path u
        | In_situ_file_page _ -> assert false
      )
    |> Lwt.join

end


    (* let page, path = *)
    (*   match u, in_situ with *)
    (*   | Guizmin.Workflow.Extract (v, p), true -> *)
    (*     let occupied = *)
    (*       match Hashtbl.find pages path with *)
    (*       | { kind = File_page w } -> v <> w *)
    (*       | _ -> true *)
    (*       | exception Not_found -> *)
    (*         let page = { path ; kind = File_page v } in *)
    (*         add_page page path ; *)
    (*         false *)
    (*     in *)
    (*     if occupied then ( *)
    (*       let msg = *)
    (*         sprintf *)
    (*           "Cannot create page %s because %s is already occupied" *)
    (*           (string_of_path (path @ p)) *)
    (*           (string_of_path path) *)
    (*       in *)
    (*       failwith msg *)
    (*     ) *)
    (*     else ( *)
    (*       let path = path @ p in *)
    (*       let page = { path ; kind = In_situ_file_page } in *)
    (*       page, path *)
    (*     ) *)
    (*   | _ -> *)
    (*     { path ; kind = File_page u }, path *)
    (* in *)
    (* add_page page path ; *)
    (* page *)

(*
    (* let add_page page path = *)
  (*   if Hashtbl.mem pages path then ( *)
  (*     let path = string_of_path path in *)
  (*     let msg = sprintf "Guizmin.Website: attempt to create page %s twice" path in *)
  (*     failwith msg *)
  (*   ) ; *)
  (*   Hashtbl.add pages path page *)






  let rec prefix_diff ~prefix p = match prefix, p with
    | [], p -> Some p
    | _, [] -> None
    | h :: t, h' :: t' ->
      if h = h' then prefix_diff ~prefix:t t' else None

  (* Find file pages whose workflow contains the workflow Extract (u, p) *)
  let find_strict_containers pages u p =
    if p = [] then []
    else (
      List.fold
        ~f:(fun accu (_, page) ->
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
        pages ~init:[]
    )

  let find_largest_strict_container pages u p =
    Core.Core_list.reduce
      (find_strict_containers pages u p)
      ~f:(fun ((_,p) as x) ((_,q) as y) ->
          if List.length p < List.length q then x else y)

  (* let classify_page page = match page.kind with *)
  (*   | Html_page _ -> `Html_page *)
  (*   | File_page (Guizmin.Workflow.Extract (v, p) as u) -> ( *)
  (*       match find_largest_strict_container v p with *)
  (*       | None -> `Root_file u *)
  (*       | Some (root, q) -> `Leaf_file (root, q) *)
  (*     ) *)
  (*   | File_page u -> `Root_file u *)
  (*   | In_situ_file_page (_,_) -> `In_situ_file_page *)

  let path_conflict = const false

  let rec pairs_exists x ~f = match x with
    | h :: t ->
      if List.exists t ~f:(f h) then true
      else pairs_exists t ~f
    | [] -> false

  let in_situ_completion pages =
    List.fold pages ~init:pages ~f:(fun pages p -> match p.kind with
        | In_situ_file_page (u, path_u, path_in_u) ->
          List.find pages ~f:(fun q -> match q.kind
      )
  let generate ~dir pages =
*)
