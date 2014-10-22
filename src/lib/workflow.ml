open Core.Std
open Defs

type 'a t = u
and u =
  | Input of path
  | Extract of u * path
  | Step of step
and step = {
  deps : u list ;
  script : cmd list ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int ; (** Maximum allowed running time in hours *)
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
  type bash_script = ([`bash_script], [`text]) file

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

let string_of_token target build_target = function
  | S s -> s
  | D w -> string_of_path (target (w :> u))
  | T -> build_target

let string_of_cmd target build_target tokens =
  List.map tokens ~f:(string_of_token target build_target)
  |> String.concat

let shell_script target build_target r =
  List.map r.script ~f:(string_of_cmd target (string_of_path build_target))

let step ?(np = 1) ?(mem = 100) ?(timeout = 24) script =
  let deps = deps_of_cmds script in
  Step { deps ; script ; np ; mem ; timeout }

let extract u path = match u with
  | Extract (v, p) -> Extract (v, p @ path)
  | Input _ | Step _ -> Extract (u, path)

let input target = Input (path_of_string target)


module API = struct
  type shell_expr = token list

  let workflow = step

  let program ?path p ?stdin ?stdout ?stderr args =
    let path_expr =
      match path with
      | None | Some [] -> []
      | Some pkgs ->
        S "PATH=" ::
        (
          List.map pkgs ~f:(fun p -> [ D p ; S "/bin" ])
          |> List.intersperse ~sep:[S ":"]
          |> List.concat
        )
        @ [ S ":$PATH" ]
    in
    let prog_expr = [ S p ] in
    let stdout_expr =
      match stdout with
      | None -> []
      | Some e -> S " > " :: e
    in
    let stdin_expr =
      match stdin with
      | None -> []
      | Some e -> S " < " :: e
    in
    let stderr_expr =
      match stderr with
      | None -> []
      | Some e -> S " 2> " :: e
    in
    [ path_expr ; prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
    |> List.filter ~f:(( <> ) [])
    |> List.intersperse ~sep:[S " "]
    |> List.concat

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

  let seq xs = List.concat xs

  let enum dic x = [ S (List.Assoc.find_exn dic x) ]

  let opt o f x = S o :: S " " :: f x

  let flag f x b = if b then f x else []

  let mkdir d = program "mkdir" [ d ]

  let mkdir_p d = program "mkdir" [ string "-p" ; d ]

  let cd p = program "cd" [ p ]

  let rm_rf x = program "rm" [ string "-rf" ; x ]

  let mv x y = program "mv" [ x ; y ]

  let wget url = program "wget" [ string url ]

  let bash ?path script ?stdin ?stdout ?stderr args =
    program "bash" ?path ?stdin ?stdout ?stderr (dep script :: args)

  let ( // ) x y = x @ [ S "/" ; S y ]
end

let deps = function
  | Input _ -> []
  | Step s -> s.deps
  | Extract (u,_) -> [ u ]
