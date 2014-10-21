open Core.Std

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

type path = string list

type 'a t = u
and u = {
  target : path ;
  deps : u list ;
  script : cmd list ;
}
and cmd = token list
and token =
  | S : string -> token
  | D : _ t -> token
  | T : token

module Types = struct
  type 'a workflow = 'a t

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type package = [`package] directory

  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file

  class type ['a, 'b, 'c, 'd] tabular = object
    inherit [[`tabular], [`text]] file
    method columns : 'a
    method header : [< `yes | `no] as 'b
    method sep : 'c
    method comment : 'd
  end

  type ('a, 'b, 'c) tsv = ('a, 'b, [`tab], 'c) tabular

end

let deps_of_cmd l =
  List.filter_map l ~f:(function
      | D r -> Some (r :> u)
      | S _ | T -> None
    )
  |> List.dedup

let deps_of_cmds l =
  List.fold_left l ~init:[] ~f:(fun accu cmd ->
      List.dedup (deps_of_cmd cmd @ accu)
    )

let string_of_path p =
  List.fold_left p ~init:"" ~f:Filename.concat

let string_of_token target = function
  | S s -> s
  | D r -> string_of_path r.target
  | T -> string_of_path target

let string_of_cmd target tokens =
  List.map tokens ~f:(string_of_token target)
  |> String.concat

let make ?target script =
  let target = match target with
    | Some p -> p
    | None ->
      let id = digest script in
      [ ".guizmin" ; id ]
  in
  let deps = deps_of_cmds script in
  { target ; deps ; script }

let in_target rule path =
  let deps = [ rule ] in
  let script = [] in
  let target = rule.target @ path in
  { target ; deps ; script }

let input target =
  { target ; script = [] ; deps = [] }

let ( >:: ) target u = { u with target }

module API = struct
  type expr = token list

  let workflow = make

  let program ?path p ?stdin ?stdout ?stderr args =
    List.concat (
      (
        match path with
        | None | Some [] -> []
        | Some pkgs ->
          [ S ("PATH=" ^ String.concat ~sep:":" (List.map pkgs ~f:(fun p -> string_of_path (p.target @ [ "bin" ])))) ]
      )
      ::
      [ S p ]
      :: args
      @ (
        match stdout with
        | None -> []
        | Some e -> [ S " > " :: e ])
      @ (
        match stdin with
        | None -> []
        | Some e -> [ S " < " :: e ])
      @ (
        match stderr with
        | None -> []
        | Some e -> [S " 2> " :: e])
    )
    |> List.intersperse ~sep:(S " ")


  let target () = [ T ]
  let string s = [ S s ]
  let int i = [ S (string_of_int i) ]
  let float f = [ S (Float.to_string f) ]
  let path p = [ S (string_of_path p) ]
  let dep w = [ D w ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:[ S "," ]
    |> List.concat

  let enum dic x = [ S (List.Assoc.find_exn dic x) ]

  let opt o f x = S o :: S " " :: f x

  let mkdir d = program "mkdir" [ d ]

  let mkdir_p d = program "mkdir" [ string "-p" ; d ]
end
