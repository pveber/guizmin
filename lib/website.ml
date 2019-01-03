(* open Core *)

(* module App = Bistro_app
 * 
 * type path = string list
 * let string_of_path = Bistro.Path.to_string
 * 
 * type html
 * 
 * type 'a build = 'a with_deps App.t
 * 
 * and 'a with_deps = {
 *   deps : any_page list ;
 *   value : 'a ;
 * }
 * 
 * and any_page = {
 *   path : path ;
 *   contents : contents ;
 * }
 * 
 * and contents =
 *   | Html of Tyxml_html.doc
 *   | Bistro_output of string
 * 
 * type 'a page = any_page
 * 
 * type 'a data = 'a Bistro_app.path
 * 
 * type 'a link = path
 * 
 * let pure x = App.pure {
 *   deps = [] ;
 *   value =  x ;
 * }
 * 
 * let workflow w =
 *   let open App in
 *   app
 *     (pure (fun x -> { deps = [] ; value = x }))
 *     (pureW w)
 * 
 * (\* let link p = *\)
 * (\*   let open App in *\)
 * (\*   app *\)
 * (\*     (pure (fun (Page x) -> { deps = [ p ] ; value = x.path })) *\)
 * (\*     (pure p) *\)
 * 
 * 
 * let add_dep accu d =
 *   d :: accu
 * (\* FIXME !!! *\)
 * 
 * let deps_union x y =
 *   List.fold x ~init:y ~f:add_dep
 * 
 * let app f x =
 *   let open App in
 *   pure (fun f x -> { deps = deps_union f.deps x.deps ;
 *                      value = f.value x.value })
 *   $ f
 *   $ x
 * 
 * let list xs =
 *   let open App in
 *   pure (
 *     fun xs -> {
 *         deps = List.fold xs ~init:[] ~f:(fun accu x -> deps_union x.deps accu ) ;
 *         value = List.map xs ~f:(fun x -> x.value) ;
 *       }
 *   )
 *   $ list xs
 * 
 * let assoc xs =
 *   let zip builds = List.map2_exn xs builds ~f:(fun (k, _) v -> k, v) in
 *   let builds = List.map xs ~f:snd |> list in
 *   let open App in
 *   pure (fun builds -> { builds with value = zip builds.value })
 *   $ builds
 * 
 * let option = function
 *   | None -> pure None
 *   | Some b -> app (pure (fun x -> Some x)) b
 * 
 * let link b =
 *   let open App in
 *   let f { value = p ; deps } =
 *     { deps = add_dep deps p ;
 *       value = p.path }
 *   in
 *   app (pure f) b
 * 
 * module Infix = struct
 *   let ( $ ) = app
 *   let ( >>| ) x f = app (pure f) x
 * end
 * 
 * let html path elt =
 *   { path ; contents = Html elt }
 * 
 * let data ?path w =
 *   let path = match path with
 *     | None -> [ "file" ; Bistro.Workflow.id w ]
 *     | Some p -> p
 *   in
 *   let open App in
 *   pure (
 *     fun { value = Path p } ->
 *       { value = { path ; contents = Bistro_output p } ;
 *         deps = [] }
 *   )
 *   $ workflow w
 * 
 * let href path = string_of_path path
 * 
 * let a path elts =
 *   Tyxml_html.(a ~a:[a_href (href path)] elts)
 * 
 * let a_sel dir (Bistro.Selector p) elts =
 *   let path = dir @ p in
 *   Tyxml_html.(a ~a:[a_href (string_of_path path)] elts)
 * 
 * let write_html ~dest ~path doc =
 *   let fspath = Filename.concat dest (string_of_path path) in
 *   Unix.mkdir_p (Filename.dirname fspath) ;
 *   Out_channel.with_file fspath ~f:(fun oc ->
 *       Tyxml_html.pp () (Format.formatter_of_out_channel oc) doc
 *     )
 * 
 * let symlink src dst =
 *   let create_link =
 *     if Sys.file_exists dst = `Yes then Unix.(
 *         if (lstat dst).st_kind <> S_LNK || readlink dst <> src
 *         then (
 *           unlink dst ;
 *           true
 *         )
 *         else false
 *       )
 *     else true
 *   in
 *   if create_link
 *   then
 *     Sys.command_exn (Printf.sprintf "ln -s `readlink -f %s` %s" src dst)
 * 
 * let write_bistro_output ~dest ~path fspath =
 *   let dest = Filename.concat dest (string_of_path path) in
 *   Unix.mkdir_p (Filename.dirname fspath) ;
 *   symlink fspath dest
 * 
 * let write ~dest pages =
 *   List.iter pages ~f:(function
 *       | { path ; contents = Html h } ->
 *         write_html ~dest ~path h
 *       | { path ; contents = Bistro_output fspath } ->
 *         write_bistro_output ~dest ~path fspath
 *     )
 * 
 * let generate b ~dest =
 *   let open App in
 *   let f { deps ; value = p } =
 *     write (p :: deps) ~dest
 *   in
 *   app (pure f) b
 * 
 * module Syntax = struct
 *   module Let_syntax = struct
 *     type 'a t = 'a build
 *     let map x ~f = app (pure f) x
 *     let both x y =
 *       let open App in
 *       pure (
 *         fun x y ->
 *           {
 *             deps = deps_union x.deps y.deps ;
 *             value = x.value, y.value
 *           }
 *       )
 *       $ x
 *       $ y
 *   end
 * end
 * 
 * module Make() = struct
 *   type _ page = {
 *     path : path ;
 *     kind : kind
 *   }
 *   and kind =
 *     | Html_page of Tyxml_html.doc
 *     | File_page of Bistro.u
 * 
 *   let path p = p.path
 * 
 *   let site = ref []
 * 
 *   let validate_addition site page =
 *     if List.mem site page then `already_in
 *     else
 *       let conflict page' =
 *         page.path = page'.path && page.kind <> page'.kind
 *       in
 *       if List.exists site ~f:conflict
 *       then `conflict page.path
 *       else `insert page
 * 
 * 
 *   let add_page page =
 *     match validate_addition !site page with
 *     | `already_in -> ()
 *     | `conflict p ->
 *       failwithf "Path %s is already taken" (string_of_path p) ()
 *     | `insert page ->
 *       site := page :: !site
 * 
 *   let fspath root x =
 *     Filename.concat root (string_of_path x)
 * 
 * 
 *   let alias fspath p1 p2 =
 *     let p2_to_p1 = List.map ~f:(fun _ -> "..") (List.tl_exn p2) @ p1 in
 *     let dst = fspath p2 in
 *     Unix.mkdir_p (Filename.dirname dst) ;
 *     ignore (Sys.command (sprintf "rm -rf %s && ln -s %s %s" dst (string_of_path p2_to_p1) dst))
 * 
 *   let html_page p html =
 *     let page = { path = p ; kind = Html_page html } in
 *     add_page page ;
 *     page
 * 
 *   let file_path u =
 *     [ "file" ; Bistro.Workflow.id u ]
 * 
 *   let file_page ?path w =
 *     let u = Bistro.Workflow.u w in
 *     let path = match path with
 *       | Some p -> p
 *       | None -> file_path w
 *     in
 *     let page = { path ; kind = File_page u } in
 *     add_page page ;
 *     page
 * 
 *   let href d = string_of_path (path d)
 * 
 *   let a d elts =
 *     Tyxml_html.(a ~a:[a_href (href d)] elts)
 * 
 *   let a_in_dir dir (Bistro.Selector p) elts =
 *     let path = dir.path @ p in
 *     Tyxml_html.(a ~a:[a_href (string_of_path path)] elts)
 * 
 *   let href_in_dir dir (Bistro.Selector p) =
 *     let path = dir.path @ p in
 *     string_of_path path
 * 
 * end *)

(* module App = Bistro_app *)
