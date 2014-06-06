(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Guizmin

open Experiment_description

let chIP_pho4_noPi = {
  sample_id = "chIP_pho4_noPi" ;
  sample_type = `short_reads `sra ;
  sample_exp = `FAIRE ;
  sample_files = [
    "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/SRR217/SRR217304/SRR217304.sra" ;
    "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/SRR217/SRR217305/SRR217305.sra" ;
  ] ;
  sample_model = "sacCer" ;
  sample_condition = "noPi" ;
}

let noPi = "noPi"
let sacCer = {
  model_id = "sacCer" ;
  model_genome = `ucsc `sacCer2
}

let config = [
  Condition noPi ;
  Model sacCer ;
  Sample chIP_pho4_noPi ;
]


open Cmdliner

let version = "0.1"

let save_config_cmd =
  let output =
    let doc = "Output path for saving the configuration file" in
    let docv = "OUTPUT" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
  in
  Term.(pure (Experiment_description.save config) $ output),
  Term.info "saveconfig" ~version


let default_cmd =
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "sacCer2" ~version

let cmds = [ default_cmd ; save_config_cmd ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0


