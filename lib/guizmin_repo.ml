open Core.Std
open Printf

type path = string list
type html_elt = [`Html] Html5.M.elt


type error = [
    `Workflow_error of (Bistro.Workflow.u * string) list
  | `Failure of string
]


module type Engine = sig
  val root : string
  val build : Bistro.Workflow.u -> [ `Ok of string | `Error of error ] Lwt.t
end

module R = Pvem.With_deferred(Lwt)

open R

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

let string_of_path = Misc.string_of_path

module Make(E : Engine) = struct
  type _ page = {
    path : path ;
    kind : kind
  }
  and kind =
    | Html_page of html_elt
    | File_page of Bistro.Workflow.u

  let path p = p.path

  let site = ref []

  let validate_addition site page =
    if List.mem site page then `already_in
    else
      let conflict page' =
        page.path = page'.path && page.kind <> page'.kind
      in
      if List.exists site ~f:conflict
      then `conflict page.path
      else `insert page


  let add_page site page =
    match validate_addition site page with
    | `already_in -> `Ok site
    | `conflict p ->
      `Error (`Failure (sprintf "Path %s is already taken" (string_of_path p)))
    | `insert page -> `Ok (page :: site)

  let fspath x =
    Filename.concat E.root (string_of_path x)


  let alias fspath p1 p2 =
    let p2_to_p1 = List.map ~f:(fun _ -> "..") (List.tl_exn p2) @ p1 in
    let dst = fspath p2 in
    Unix.mkdir_p (Filename.dirname dst) ;
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

  let html_page p html =
    let page = { path = p ; kind = Html_page html } in
    let fspath = fspath (path page) in
    Unix.mkdir_p (Filename.dirname fspath) ;
    Core.Out_channel.with_file fspath ~f:(fun oc ->
        Html5.P.print ~output:(output_string oc) html
      ) ;
    add_page !site page |> of_result >>= fun site' ->
    site := site' ;
    return page

  let file_path u =
    [ "file" ; Bistro.Workflow.id' u ]

  let file_page ?path w =
    let u = Bistro.Workflow.((w : _ t :> u)) in
    let path = match path with
      | Some p -> p
      | None -> file_path u
    in
    let page = { path ; kind = File_page u } in
    add_page !site page |> of_result >>= fun site' ->
    site := site' ;
    let file_fspath = fspath (file_path u) in
    Unix.mkdir_p (Filename.dirname file_fspath) ;
    E.build u >>= fun cache_fspath ->
    symlink cache_fspath file_fspath ;
    if path <> file_path u then alias fspath (file_path u) path ;
    return page

  let href d = string_of_path (path d)

  let a d elts =
    Html5.M.(a ~a:[a_href (href d)] elts)

  let a_in_dir dir p elts =
    let path = dir.path @ p in
    let fspath = fspath path in
    if Sys.file_exists_exn fspath then
      `Ok Html5.M.(a ~a:[a_href (string_of_path path)] elts)
    else
      let msg =
        sprintf
          "Path %s doesn't exist in dir %s"
          (string_of_path p)
          (string_of_path dir.path)
      in
      `Error (`Failure msg)

end
