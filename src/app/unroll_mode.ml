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
      x,
      Bistro_repo.item
        [ "aligned_reads" ; (x # sample).sample_id ]
        (W.DNA_seq_with_reference.aligned_reads_indexed_bam x)
    )

  let lsnd = List.map ~f:snd

  let repo = [
    fastQC_reports_items ;
    bam_bai_items_of_short_reads_samples_with_reference |> lsnd
  ]
  |> List.concat
  |> Bistro_repo.make



  let k = Html5.M.pcdata

  let html_page page_title contents = Html5.M.(
    let head =
      head (title (pcdata page_title)) [
  	link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
  	link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
      ] in
    let scripts = [
      script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
      script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
    ] in
    html head (body (div ~a:[a_class ["container"]] contents :: scripts))
  )

  let link_of_path text path =
    Html5.M.(a ~a:[a_href (String.concat ~sep:"/" path) ] [ pcdata text ])

  let link_of_item text = function Bistro_repo.Item (_,_,path) ->
    link_of_path text path

  let custom_track_link_of_bam_bai_item webroot ucsc_genome sample_id (Bistro_repo.Item (_,_,path)) =
    let local_path = String.concat ~sep:"/" path in
    let name = sample_id ^ " aligned_reads" in
    let opts = [
      `track_type "bam" ;
      `bigDataUrl (webroot ^ "/" ^ local_path ^ "/reads.bam") ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Guizmin.Ucsc_gb.CustomTrack.url ucsc_genome opts in
    Html5.M.(a ~a:[a_href url] [ pcdata sample_id ])

  (* let custom_track_link_of_bigwig_item webroot = *)
  (*   function (sample, Guizmin_repo.Item (_,_,path)) -> *)
  (*     let local_path = String.concat ~sep:"/" path in *)
  (*     let name = sample.sample_id ^ " signal" in *)
  (*     let opts = [ *)
  (* 	`track_type "bigWig" ; *)
  (* 	`bigDataUrl (webroot ^ "/" ^ local_path) ; *)
  (*   	`color (0,0,255) ; *)
  (*   	`visibility `dense ; *)
  (*   	`name name ; *)
  (*   	`description name ; *)
  (*     ] *)
  (*     in *)
  (*     let url = Ucsc.CustomTrack.url (model sample.sample_model).model_genome opts in *)
  (*     Html5.M.(a ~a:[a_href url] [ pcdata sample.sample_id ]) *)

  let string_of_experiment = function
    | `whole_cell_extract -> "WCE"
    | `TF_ChIP tf -> Printf.sprintf "ChIP-seq (%s)" tf
    | `FAIRE -> "FAIRE"
    | `mRNA -> "mRNA"

  let link_table filter link_of_item collections =
    let open Html5.M in
    let links =
      List.concat collections
      |> List.filter ~f:(fun (s,_) -> filter s)
      |> List.map ~f:(fun ((s,item) as e) -> (s, link_of_item e))
    in
    let header = tr [ th [ k "Model" ] ; th [ k "Condition" ] ; th [ k "Experiment" ] ; th [ k "Sample" ] ] in
    let lines = List.map links ~f:(fun (s,link) ->
      tr [
  	td [ k s.sample_model ] ;
  	td [ k s.sample_condition ] ;
  	td [ k (string_of_experiment s.sample_exp) ] ;
  	td [ link ] ;
      ]
    )
    in
    table ~a:[a_class ["table"]] header lines

  let filter_ucsc_samples = List.filter_map ~f:(fun (s,item) ->
    match (s # genomic_reference : W.Genome.t :> genome) with 
    | `ucsc genome -> Some (s#sample, (genome, (s#sample).sample_id, item))
    | _ -> None
					   )

  let index_custom_tracks_section webroot =
    let open Html5.M in
    let aligned_reads_link_table =
      link_table
  	(const true)
  	(fun (sample, (sample_id, genome, item)) -> custom_track_link_of_bam_bai_item webroot sample_id genome item)
  	[filter_ucsc_samples bam_bai_items_of_short_reads_samples_with_reference]
    in
    (* let signal_link_table = *)
    (*   link_table *)
    (* 	(const true) *)
    (* 	(custom_track_link_of_bigwig_item webroot) *)
    (* 	[ chipseq_bigwig_items ; wceseq_bigwig_items ; faireseq_bigwig_items (\* ; rnaseq_bigwig_items *\) ] *)
    (* in *)
    div [
      h2 ~a:[a_id "custom-tracks"] [k "UCSC Genome Browser custom tracks"] ;
      p [k "The datasets can be visualized on the " ;
  	 a ~a:[a_href "http://genome.ucsc.edu/cgi-bin/hgTracks"] [k"UCSC Genome Browser"] ;
  	 k ". To achieve this, simply click on the link corresponding to the sample you want to visualize." ;
  	 k " In order to keep a particular combination of custom tracks on the browser, consider using " ;
  	 a ~a:[a_href "http://genome.ucsc.edu/goldenPath/help/hgSessionHelp.html"] [k"sessions"] ;
  	 k"."
  	] ;
      h3 ~a:[a_id "custom-tracks-aligned-reads"] [k "Aligned reads"] ;
      p [k "These tracks display the raw alignments of reads from HTS samples. "] ;
      aligned_reads_link_table ;
      (* h3 ~a:[a_id "custom-tracks-signal"] [k "Signal"] ; *)
      (* p [k "These tracks display the raw signal from HTS samples. "] ; *)
      (* signal_link_table ; *)
    ]


  (* let index_quality_control_section () = *)
  (*   let open Html5.M in *)
  (*   let fastQC_reports_table = *)
  (*     link_table *)
  (* 	(const true) *)
  (* 	(fun (s,_) -> link_of_path s.sample_id ["quality_control" ; "FastQC" ; s.sample_id]) *)
  (* 	[ List.map samples (fun s -> s, ()) ] *)
  (*   in *)
  (*   div [ *)
  (*     h2 ~a:[a_id "quality-controls"] [k"Quality controls"] ; *)
  (*     p [k "The following table provides links to FastQC reports to assess the quality of each HTS sample."] ; *)
  (*     fastQC_reports_table ; *)
  (*   ] *)

  let index webroot =
    let open Html5.M in html_page "Guizmin workflow" [
      h1 [b [k"Guizmin_workflow"]] ;
      hr () ;
      (* index_quality_control_section () ; *)
      index_custom_tracks_section webroot ;
    ]

end

let main ged_file output webroot = Guizmin_workflow.(
  let description = Experiment_description.load ged_file in
  let module W = (val Unroll_workflow.from_description description) in
  let module WWW = Make_website(W) in
  let db = Bistro_db.make "_guizmin" in
  let () = Bistro_db.setup db in
  let logger = Bistro_logger.make () in
  let logger_thread = Lwt_stream.iter_s Lwt_io.printl (Lwt_react.E.to_stream (Bistro_logger.to_strings logger)) in
  let backend = Bistro_concurrent.local_worker ~np:6 ~mem:(6 * 1024) in
  let t = Bistro_concurrent.build_repo ~base:output ~wipeout:true db logger backend WWW.repo in
  Lwt_unix.run t ;
  let path = Filename.concat output "index.html" in
  Out_channel.with_file path ~f:(fun oc ->
    Html5.P.print ~output:(output_string oc) (WWW.index webroot)
  )

)
