open Core.Std

module App = Bistro_app

type path = string list
let string_of_path = Bistro.string_of_path

type html

type 'a build = {
  deps : any_page list ;
  value : 'a App.t ;
}

and 'a page = {
  path : path ;
  contents : 'a contents
}

and 'a contents =
  | Html : Tyxml_html.doc -> html contents
  | Data : 'a App.path -> 'a App.path contents

and any_page = Page : _ page -> any_page

type 'a link = path
type 'a data = 'a Bistro_app.path

let html_page path elt =
  let p = { path ; contents = Html elt } in
  { deps = [ Page p ] ; value = App.pure p }


let data_page path fspath =
  let p = { path ; contents = Data fspath } in
  { deps = [ Page p ] ; value = App.pure p }

(* let with_page page f = *)
(*   let res = f page.value.path in *)
(*   { res with deps = Page page.value :: res.deps } *)

(* let with_link path f g = *)
(*   let y = g path in *)
(*   let page = html_page path (f y.value) in *)
(*   { page with deps = page.deps @ y.deps } *)

let return _ = assert false
let bind _ = assert false
let bindP _ = assert false
let bindD _ = assert false
let bindL _ = assert false

let map _ = assert false
let mapP _ = assert false
let mapD _ = assert false

let a _ = assert false

let a_sel _ = assert false

let href _ = assert false

let generate _ = assert false



module Make() = struct
  type _ page = {
    path : path ;
    kind : kind
  }
  and kind =
    | Html_page of Tyxml_html.doc
    | File_page of Bistro.u

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


  let add_page page =
    match validate_addition !site page with
    | `already_in -> ()
    | `conflict p ->
      failwithf "Path %s is already taken" (string_of_path p) ()
    | `insert page ->
      site := page :: !site

  let fspath root x =
    Filename.concat root (string_of_path x)


  let alias fspath p1 p2 =
    let p2_to_p1 = List.map ~f:(fun _ -> "..") (List.tl_exn p2) @ p1 in
    let dst = fspath p2 in
    Unix.mkdir_p (Filename.dirname dst) ;
    ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))

  let html_page p html =
    let page = { path = p ; kind = Html_page html } in
    add_page page ;
    page

  let file_path u =
    [ "file" ; Bistro.Workflow.id u ]

  let file_page ?path w =
    let u = Bistro.Workflow.u w in
    let path = match path with
      | Some p -> p
      | None -> file_path w
    in
    let page = { path ; kind = File_page u } in
    add_page page ;
    page

  let href d = string_of_path (path d)

  let a d elts =
    Tyxml_html.(a ~a:[a_href (href d)] elts)

  let a_in_dir dir (Bistro.Selector p) elts =
    let path = dir.path @ p in
    Tyxml_html.(a ~a:[a_href (string_of_path path)] elts)

  let href_in_dir dir (Bistro.Selector p) =
    let path = dir.path @ p in
    string_of_path path

end

(* module App = Bistro_app *)

