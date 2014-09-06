(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Core.Std
open Guizmin

open Experiment_description

let chIP_pho4_noPi = {
  sample_id = "chIP_pho4_noPi" ;
  sample_type = `short_reads `sra ;
  sample_exp = `TF_ChIP "Pho4" ;
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
  model_genome = Some (`ucsc `sacCer2)
}

let config = [
  Project "sacCer2_pho4" ;
  Condition noPi ;
  Model sacCer ;
  Sample chIP_pho4_noPi ;
]

let build_repo repo label =
  let db = Bistro_db.init "_guizmin" in
  let blog = Bistro_log.make ~db () in
  let log_event, send_to_log_event = React.E.create () in
  let _ = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream log_event) in
  let backend = Bistro_engine_lwt.local_worker ~np:6 ~mem:(6 * 1024) blog in
  let t = Bistro_engine_lwt.build_repo ~base:(Filename.concat "sacCer2" label) ~wipeout:false db blog backend repo in
  Lwt_unix.run t

let macs_vs_macs2 () =
  let module W = (val Unroll_workflow.from_description config) in
  let repo = Bistro_repo.make (List.concat [
      List.map W.tf_chip_seq_samples ~f:(fun s ->
          let w = Macs.No_control.run ~gsize:(`gsize 12100000) ~pvalue:1e-5 s#aligned_reads_bam in
          Bistro_repo.item ["peaks";"macs"] w
        )
    ])
  in
  build_repo repo "macs_vs_macs2"

open Cmdliner

let version = "0.1"

let macs_vs_macs2_cmd =
  Term.(pure macs_vs_macs2 $ pure ()),
  Term.info "macs_vs_macs2" ~version

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
  Term.info "sacCer2_pho4" ~version

let cmds = [ save_config_cmd ; macs_vs_macs2_cmd ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0


