open Core.Std
open Common

module type Params = sig
  val workflow_output : Bistro_workflow.u -> string Lwt.t
  val output_dir : string
  val webroot : string
end

type document = {
  path : string list ; (* path in the website *)
  dst : string ; (* path of the document on the file system *)
  contents : [
    | `Page of [`Html] Html5.M.elt
    | `Raw of Bistro_workflow.u
  ]
}


module Make_website(W : Guizmin.Unrolled_workflow.S)(P : Params) = struct
  open P
  open Guizmin
  open Experiment_description

  let workflow_output' w = workflow_output (Bistro_workflow.u w)

  let string_of_path l = String.concat ~sep:"/" l

  let dst path = Filename.concat output_dir (string_of_path path)

  let symlink src dst =
    let create_link =
      if Sys.file_exists dst = `Yes then Unix.(
          if (lstat dst).st_kind <> S_LNK || readlink dst <> src
          then (
            unlink dst ;
            true
          )
          else false
        )
      else true
    in
    if create_link
    then
      Sys.command_exn (Printf.sprintf "ln -s `readlink -f %s` %s" src dst)

  let page path page =
    let dst = dst path in
    mkdir_p (Filename.dirname dst) ;
    Out_channel.with_file dst ~f:(fun oc ->
        Html5.P.print ~output:(output_string oc) page
      ) ;
    { path ; dst ; contents = `Page page }

  let raw ?(cherry_pick = false) path w =
    lwt fn = workflow_output' w in
    let dst_base = dst path in
    lwt path = match cherry_pick, Bistro_workflow.u w with
      | false, Bistro_workflow.Select (u, path_in_u) ->
        lwt fn_u = workflow_output u in
        mkdir_p (Filename.dirname dst_base) ;
        symlink fn_u dst_base ;
        Lwt.return (path @ [path_in_u])
      | _ ->
        mkdir_p (Filename.dirname dst_base) ;
        symlink fn dst_base ;
        Lwt.return path
    in
    Lwt.return { path ;
                 dst = dst path ;
                 contents = `Raw (Bistro_workflow.u w) }

  let fastQC_reports =
    let open W.Short_read_sample in
    List.bind list (fun x ->
        List.mapi (fastQC_report x) ~f:(fun i report ->
            raw
              ["quality_control" ; "FastQC" ; x#sample.sample_id ; string_of_int i ]
              report
          )
      )

  let bam_bai_of_short_reads_samples_with_reference =
    List.map W.DNA_seq_with_reference.list ~f:(fun x ->
      x,
      raw
        [ "aligned_reads" ; (x # sample).sample_id ]
        (W.DNA_seq_with_reference.aligned_reads_indexed_bam x)
    )

  let lsnd = List.map ~f:snd


  let k = Html5.M.pcdata

  let html_page page_title contents =
    let open Html5.M in
    let head =
      head (title (pcdata page_title)) [
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
      ] in
    let scripts = [
      script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
      script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
    ]
    in
    html head (body (div ~a:[a_class ["container"]] contents :: scripts))

  let link_of_path text path =
    Html5.M.(a ~a:[a_href (String.concat ~sep:"/" path) ] [ pcdata text ])

  let link_of_doc text d =
    link_of_path text d.path

  let custom_track_link_of_bam_bai ucsc_genome sample_id d =
    let local_path = String.concat ~sep:"/" d.path in
    let name = sample_id ^ " aligned_reads" in
    let opts = [
      `track_type "bam" ;
      `bigDataUrl (webroot ^ "/" ^ local_path ^ "/reads.bam") ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Gzt.Ucsc_gb.CustomTrack.url ucsc_genome opts in
    Html5.M.(a ~a:[a_href url] [ pcdata sample_id ])

  (* let custom_track_link_of_bigwig_item webroot = *)
  (*   function (sample, Guizmin_repo.Item (_,_,path)) -> *)
  (*     let local_path = String.concat ~sep:"/" path in *)
  (*     let name = sample.sample_id ^ " signal" in *)
  (*     let opts = [ *)
  (* `track_type "bigWig" ; *)
  (* `bigDataUrl (webroot ^ "/" ^ local_path) ; *)
  (*   `color (0,0,255) ; *)
  (*   `visibility `dense ; *)
  (*   `name name ; *)
  (*   `description name ; *)
  (*     ] *)
  (*     in *)
  (*     let url = Ucsc.CustomTrack.url (model sample.sample_model).model_genome opts in *)
  (*     Html5.M.(a ~a:[a_href url] [ pcdata sample.sample_id ]) *)

  let string_of_experiment = function
    | `whole_cell_extract -> "WCE"
    | `TF_ChIP tf -> Printf.sprintf "ChIP-seq (%s)" tf
    | `FAIRE -> "FAIRE"
    | `mRNA -> "mRNA"

  let link_table filter link_of_sample_doc collections =
    let open Html5.M in
    let links =
      List.concat collections
      |> List.filter ~f:(fun (s,_) -> filter s)
      |> List.map ~f:(fun ((s,_) as e) -> (s, link_of_sample_doc e))
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

  let index_custom_tracks_section =
    let open Html5.M in
    lwt ucsc_samples =
      Lwt_list.map_p
        (fun (sample, (sample_id, genome, doc)) -> lwt doc = doc in Lwt.return (sample, (sample_id, genome, doc)))
        (filter_ucsc_samples bam_bai_of_short_reads_samples_with_reference)
    in
    let aligned_reads_link_table =
      link_table
        (const true)
        (fun (sample, (sample_id, genome, item)) -> custom_track_link_of_bam_bai sample_id genome item)
        [ucsc_samples]
    in
    (* let signal_link_table = *)
    (*   link_table *)
    (* (const true) *)
    (* (custom_track_link_of_bigwig_item webroot) *)
    (* [ chipseq_bigwig ; wceseq_bigwig ; faireseq_bigwig (\* ; rnaseq_bigwig *\) ] *)
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
  |> Lwt.return


  (* let index_quality_control_section () = *)
  (*   let open Html5.M in *)
  (*   let fastQC_reports_table = *)
  (*     link_table *)
  (* (const true) *)
  (* (fun (s,_) -> link_of_path s.sample_id ["quality_control" ; "FastQC" ; s.sample_id]) *)
  (* [ List.map samples (fun s -> s, ()) ] *)
  (*   in *)
  (*   div [ *)
  (*     h2 ~a:[a_id "quality-controls"] [k"Quality controls"] ; *)
  (*     p [k "The following table provides links to FastQC reports to assess the quality of each HTS sample."] ; *)
  (*     fastQC_reports_table ; *)
  (*   ] *)

  let index =
    let open Html5.M in
    lwt index_custom_tracks_section = index_custom_tracks_section in
    html_page "Guizmin workflow" [
      h1 [b [k"Guizmin_workflow"]] ;
      hr () ;
      (* index_quality_control_section () ; *)
      index_custom_tracks_section ;
    ]
  |> page ["index.html"]
  |> Lwt.return

  let generate () =
    Lwt.map ignore index
end

let make_website (module W : Guizmin.Unrolled_workflow.S) workflow_output ~output_dir ~webroot =
  let module P = struct
    let workflow_output = workflow_output
    let output_dir = output_dir
    let webroot = webroot
  end in
  let module WWW = Make_website(W)(P) in
  mkdir_p output_dir ;
  WWW.generate ()


let main opts ged_file output_dir webroot = Guizmin.(
  let description = Experiment_description.load ged_file in
  let module W = (val Unroll_workflow.from_description description) in
  let log_event, send_to_log_event = React.E.create () in
  let db = Bistro_db.init "_guizmin" in
  let blog = Bistro_log.make ~hook:send_to_log_event ~db () in
  let backend = Bistro_engine_lwt.local_worker ~np:6 ~mem:(6 * 1024) blog in
  let daemon = Bistro_engine_lwt.Daemon.make db blog backend in
  let () = if opts.verbosity = Verbose then (
      Lwt_stream.iter_s
        (fun e -> Lwt_io.printl (Bistro_log.Entry.to_string e))
        (Lwt_react.E.to_stream log_event)
      |> ignore
    )
  in
  let workflow_output u =
    let open Option in
    Lwt.bind
      (Option.value_exn (Bistro_engine_lwt.Daemon.send' daemon u))
      (fun () -> Lwt.return (Bistro_db.path db u))
  in
  let t = make_website (module W) workflow_output ~output_dir ~webroot in
  let finish_pending_jobs = Bistro_engine_lwt.Daemon.shutdown daemon in
  Lwt_unix.run (Lwt.join [ t ; finish_pending_jobs ])
)
