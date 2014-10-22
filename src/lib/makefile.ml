open Core.Std
open Defs

type path = string list

type rule = {
  target : path ;
  deps : path list ;
  body : string list ;
  phony : bool ;
  desc : string option ;
}

type t = rule list

let help_rule other_rules =
  let msg = "Prints this help" in
  let help_topics =
    ("help", msg) ::
    List.filter_map other_rules ~f:(function
        | { desc = Some text ; target } -> Some (string_of_path target, text)
        | _ -> None
      ) in
  let sorted_help_topics = List.sort ~cmp:compare help_topics in
  {
    target = [ "help" ] ;
    deps = [] ;
    body = List.map sorted_help_topics ~f:(fun (target, desc) ->
        let space_length = max 0 (30 - String.length target) in
        let space = String.make space_length ' ' in
        sprintf "@echo \"%s ::%s%s\"" target space desc
      ) ;
    phony = true ;
    desc = Some msg ;
  }

let empty = [ help_rule [] ]

let rec classify_rule mf r = match mf with
  | [] -> `compatible
  | h :: t ->
    if h.target = r.target then
      if h = r then `duplicate
      else `incompatible
    else classify_rule t r

let add_rule mf r =
  match classify_rule mf r with
  | `compatible -> r :: mf
  | `duplicate -> mf
  | `incompatible ->
    failwith (sprintf "Target %s has two different rules" (string_of_path r.target))

let rec target_of_workflow =
  let open Workflow in
  function
  | Input x -> x
  | Extract (u, p) -> target_of_workflow u @ p
  | Step r -> [ "_guizmin" ; "cache" ; digest r ]

let build_target_of_workflow r =
  [ "_guizmin" ; "build" ; digest r ]

let body_of_workflow u = match u with
  | Workflow.Input _
  | Workflow.Extract _ ->
    [ sprintf "test -e %s" (string_of_path (target_of_workflow u)) ]
  | Workflow.Step r ->
    let script_cmd =
      Workflow.shell_script target_of_workflow (build_target_of_workflow r) r
      |> String.concat ~sep:" && \\\n\t"
    in
    let mv_cmd =
      sprintf "mv %s %s"
        (string_of_path (build_target_of_workflow r))
        (string_of_path (target_of_workflow u))
    in
    [ sprintf "(%s) && \\\n\t %s" script_cmd mv_cmd ]

let rule_of_workflow u =
  {
    target = target_of_workflow u ;
    deps = List.map (Workflow.deps u) ~f:target_of_workflow ;
    body = body_of_workflow u ;
    phony = false ;
    desc = None ;
  }

let rec add_workflow_aux mf u =
  List.fold_left (Workflow.deps u) ~init:mf ~f:add_workflow_aux
  |> fun mf -> add_rule mf (rule_of_workflow u)

let add_workflow mf (w : _ Workflow.t) =
  let u = (w : _ Workflow.t :> Workflow.u) in
  add_workflow_aux mf u

let ( ++ ) mf w = add_workflow mf w

let to_channel mf oc =
  List.iter mf ~f:(fun r ->
      fprintf oc "%s: %s\n"
        (string_of_path r.target)
        (String.concat ~sep:" " (List.map r.deps ~f:string_of_path)) ;
      List.iter r.body ~f:(fprintf oc "\t%s\n") ;
      fprintf oc "\n"
    )
