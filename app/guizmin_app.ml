open Core.Std
open Common

let help copts man_format cmds topic = match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
     let topics = "topics" :: "ged" :: cmds in
     let conv, _ = Cmdliner.Arg.enum (List.rev_map ~f:(fun s -> (s, s)) topics) in
     match conv topic with
     | `Error e -> `Error (false, e)
     | `Ok t when t = "topics" -> (List.iter ~f:print_endline topics; `Ok ())
     | `Ok t when t = "ged" ->
       let contents = [
         `S "GUIZMIN EXPERIMENT DESCRIPTION FORMAT" ;
         `P "It's a file format"
       ] in
       let page = ("ged", 7, "Guizmin manual", "", "Guizmin manual"), contents in
       `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)
     | `Ok t when List.mem cmds t -> `Help (man_format, Some t)
     | `Ok _ -> assert false



open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `S "BUGS"; `P "Email bug reports to <philippe.veber@univ-lyon1.fr>.";]

let copts debug verbosity = { debug ; verbosity }
let copts_t =
  let docs = copts_sect in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verbosity =
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  Term.(pure copts $ debug $ verbosity)

(* Distribution options *)
let dopts_sect = "DISTRIBUTION OPTIONS"
let dopts backend np max_np_per_job mem =
  { backend ; np ; max_np_per_job = min np max_np_per_job ; mem }

let dopts_t =
  let docs = dopts_sect in
  let backend =
    let doc = "Backend for execution." in
    let docv = "BACKEND" in
    Arg.(value & opt (enum ["local", Local ; "pbs", Pbs]) Local & info ["backend"] ~docs ~doc ~docv)
  in
  let np =
    let doc = "Number of available processors in local mode." in
    let docv = "NP" in
    Arg.(value & opt int 1 & info ["np"] ~docs ~doc ~docv)
  in
  let max_np_per_job =
    let doc = "Maximum number of processors given to a job." in
    let docv = "MNP" in
    Arg.(value & opt int 1 & info ["mnp"] ~docs ~doc ~docv)
  in
  let mem =
    let doc = "Amount of available memory in local mode." in
    let docv = "MEM" in
    Arg.(value & opt int 4 & info ["mem"] ~docs ~doc ~docv)
  in
  Term.(pure dopts $ backend $ np $ max_np_per_job $ mem)

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about guizmin and guizmin commands" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about guizmin commands and other subjects..."] @ help_secs
  in
  Term.(ret (pure help $ copts_t $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let unroll_cmd =
  let data_description_file =
    let doc = "Path to a guizmin experiment description (.ged) file" in
    let docv = "GED_FILE" in
    Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)
  in
  let output =
    let doc = "Output directory for the pipeline, will be created if not existent. Stops if the path exists and is not an empty directory" in
    let docv = "OUTPUT" in
    Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)
  in
  let webroot =
    let doc = "webroot" in
    let docv = "WEBROOT" in
    Arg.(required & pos 2 (some string) None & info [] ~doc ~docv)
  in
  let doc = "runs a fully automated analysis of a dataset" in
  let man = [
    `S "DESCRIPTION";
    `P "Given a .ged file (read `$(mname) help ged'), guizmin-unroll will compute a suitable analysis pipeline and run it, producing its output in $(i,OUTPUT). The execution will also produce a _guizmin directory, which contains cached intermediate results." ;
  ] @ help_secs in
  Term.(pure Unroll_mode.main $ copts_t $ dopts_t $ data_description_file $ output $ webroot),
  Term.info "unroll" ~version:"0.1" ~doc ~sdocs:copts_sect ~man

let quickstart_cmd =
  let output =
    let doc = "Output path (default STDOUT). Overwrites any existing file" in
    let docv = "OUTPUT" in
    Arg.(value & opt (some string) None & info [ "o" ; "output" ] ~docv ~doc)
  in
  let doc = "Interactive construction of a ged file." in
  let man = [
    `S "DESCRIPTION";
    `P "Interactive construction of a guizmin experiment description (.ged) file."
  ] @ help_secs in
  Term.(pure Quickstart.main $ copts_t $ output),
  Term.info "quickstart" ~version:"0.1" ~doc ~sdocs:copts_sect ~man

let default_cmd =
  let doc = "a bioinformatics toolbox" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "guizmin" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

let cmds = [ help_cmd ; quickstart_cmd ; unroll_cmd ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0

(* let execute_pipeline config_file webroot = *)
(*   let config = Config_file.load config_file in *)
(*   Printf.printf "Loaded config file.\n" ; *)
(*   config, webroot *)

(* let execute_pipeline_term = Cmdliner.( *)
(*   let config_file = *)
(*     let doc = "Path to the configuration file" *)
(*     and docv = "CONFIG_FILE" in *)
(*     Arg.(required & pos 0 (some string) None & info [] ~docv ~doc) *)
(*   in *)
(*   let webroot = *)
(*     let doc = "Root URL where the repository shall be accessible" *)
(*     and docv = "WEBROOT" in *)
(*     Arg.(required & opt (some string) None & info ["webroot"] ~docv ~doc) *)
(*   in *)
(*   Term.(pure execute_pipeline $ config_file $ webroot) *)
(* ) *)

(* let info = Cmdliner.( *)
(*   let doc = "Runs a pipeline" in *)
(*   let man = [ `S "BUGS"; `P "Email bug reports to <philippe.veber@univ-lyon1.fr>.";] in *)
(*   Term.info "priba" ~version:"0.1" ~doc ~man *)
(* ) *)

(* let config, webroot = match Cmdliner.Term.eval (execute_pipeline_term,info) with *)
(*   | `Ok r -> r *)
(*   | _ -> exit 1 *)

(* module Pipeline = Pipeline.Make( *)
(*   struct *)
(*     let config_file = config *)
(*   end *)
(* ) *)

(* let repo_path = "./priba_repo" *)

(* let () = *)
(*   Guizmin_repo.( *)
(*     create *)
(*       ~np:7 ~log:stdout ~wipeout:true *)
(*       ~base:(Guizmin.default_base_directory ()) *)
(*       ~repo_base:repo_path Pipeline.repo *)
(*   ) *)

(* let () = *)
(*   let path = Filename.concat repo_path "index.html" in *)
(*   Out_channel.with_file path ~f:(fun oc -> *)
(*     Html5.P.print ~output:(output_string oc) (Pipeline.index webroot) *)
(*   ) *)