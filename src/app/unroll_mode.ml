open Core.Std

module Make_website(W : Guizmin_workflow.Unrolled_workflow.S) = struct
  open Guizmin_workflow
  open Experiment_description

  let fastQC_reports_items =
    W.Short_read_sample.(
      List.bind list (fun x ->
        List.mapi (fastQC_report x) ~f:(fun i report ->
   	  Bistro_repo.item
	    ["quality_control" ; "FastQC" ; x#sample.sample_id ; string_of_int i ]
	    report
        )
      )
    )

  let bam_bai_items_of_short_reads_samples_with_reference =
    List.map W.DNA_seq_with_reference.list ~f:(fun x ->
      Bistro_repo.item
        [ "aligned_reads" ; (x # sample).sample_id ]
        (W.DNA_seq_with_reference.aligned_reads_indexed_bam x)
    )

  let repo = [
    fastQC_reports_items ;
    bam_bai_items_of_short_reads_samples_with_reference
  ]
  |> List.concat
  |> Bistro_repo.make
end

let main ged_file output = Guizmin_workflow.(
  let description = Experiment_description.load ged_file in
  let module W = (val Unroll_workflow.from_description description) in
  let module WWW = Make_website(W) in
  let db = Bistro_db.make "_guizmin" in
  let () = Bistro_db.setup db in
  let logger = Bistro_logger.make () in
  let logger_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream (Bistro_logger.to_strings logger)) in
  let backend = Bistro_concurrent.local_worker ~np:6 ~mem:(6 * 1024) in
  let t = Bistro_concurrent.build_repo ~base:output ~wipeout:true db logger backend WWW.repo in
  Lwt_unix.run t
)
