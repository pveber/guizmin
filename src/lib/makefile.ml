open Core.Std

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
        | { desc = Some text ; target } -> Some (List.reduce_exn ~f:Filename.concat target, text)
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

let compatible_rules r r' =
  r.target <> r'.target || r = r'

let compatible_rule mf r =
  List.for_all mf ~f:(compatible_rules r)

let add_rule ?(first = false) mf r =
  if compatible_rule mf r then
    if first then r :: mf else mf @ [ r ]
  else
    failwith (sprintf "Target %s has two different rules" (List.reduce_exn ~f:Filename.concat r.target))

let rule_of_workflow u =
  {
    target = u.Workflow.target ;
    deps = List.map u.Workflow.deps ~f:(fun u -> u.Workflow.target) ;
    body = List.map u.Workflow.script ~f:(Workflow.string_of_cmd u.Workflow.target) ;
    phony = false ;
    desc = None ;
  }

let rec add_workflow_aux mf u =
  List.fold_left u.Workflow.deps ~init:mf ~f:add_workflow_aux
  |> fun mf -> add_rule mf (rule_of_workflow u)

let add_workflow ?(first = false) mf (w : _ Workflow.t) =
  let u = (w : _ Workflow.t :> Workflow.u) in
  let mf' = add_workflow_aux mf u in
  let r_u = rule_of_workflow u in
  if first then r_u :: mf' else mf' @ [ r_u ]

