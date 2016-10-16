open Core.Std

let check_errors descr =
  match Experiment_description.check descr with
  | [] -> ()
  | xs ->
    List.map xs ~f:Experiment_description.error_msg
    |> String.concat ~sep:", "
    |> failwith

let main ged dest webroot np mem () =
  let description = Experiment_description.load ged in
  check_errors description ;
  let whole_workflow = Unroll_workflow.from_description description in
  let term = Unroll_website.generate ~dest ~webroot whole_workflow in
  Bistro_app.run ~np ~mem:(mem * 1024) term

let spec =
  let open Command.Spec in
  empty
  +> flag "--ged"     (required string) ~doc:"GED Path of experiment description file"
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to generate the results"
  +> flag "--webroot" (required string) ~doc:"URL url where the generated site will be published"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"

let command =
  Command.basic
    ~summary:"Derives an analysis workflow given an experiment description"
    spec
    main
