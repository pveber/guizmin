open Core.Std
open Common

let string_of_path l = String.concat ~sep:"/" l

module type Params = sig
  val workflow_output : Bistro_workflow.u -> string Lwt.t
  val output_dir : string
  val webroot : string
end


module Make_website(W : Guizmin.Unrolled_workflow.S)(P : Params) = struct
  open P
  open Guizmin
  open Experiment_description

  (* WEBSITE GENERATION *)

  let workflow_output' w = workflow_output (Bistro_workflow.u w)

  let fs_of_w path = Filename.concat output_dir (string_of_path path)

  module Document : sig
    type t

    val web_path : t -> string list
    (** path in the website *)

    val page : string list -> [`Html] Html5.M.elt -> t Lwt.t
    val raw  : _ Bistro_workflow.t -> t Lwt.t
    val alias  : string list -> t -> t Lwt.t
    val a : t -> 'a Html5.M.elt list -> [`A of 'a] Html5.M.elt

  end
  =
  struct
    type t =
      | Page  of string list * [`Html] Html5.M.elt
      | Raw   of Bistro_workflow.u
      | Alias of string list * t

    let web_path = function
      | Page (p,_) | Alias (p,_) -> p
      | Raw u -> [ "files" ; Bistro_workflow.digest u ]

    let fs_path x =
      Filename.concat output_dir (string_of_path (web_path x))

    let page web_path elt =
      let page = Page (web_path, elt) in
      let fs_path = fs_path page in
      mkdir_p (Filename.dirname fs_path) ;
      Out_channel.with_file fs_path ~f:(fun oc ->
          Html5.P.print ~output:(output_string oc) elt
        ) ;
      Lwt.return page

    let raw w =
      let raw = Raw (Bistro_workflow.u w) in
      let fs_path = fs_path raw in
      mkdir_p (Filename.dirname fs_path) ;
      workflow_output' w >>= fun cache_path ->
      symlink cache_path fs_path ;
      Lwt.return raw

    let alias web_path doc =
      let alias = Alias (web_path, doc) in
      let alias_fs_path = fs_path alias in
      mkdir_p (Filename.dirname alias_fs_path) ;
      symlink (fs_path doc) alias_fs_path ;
      Lwt.return alias

    let a d elts =
      Html5.M.(a ~a:[a_href (string_of_path (web_path d))] elts)

  end

  let keyval_table items =
    let open Html5.M in
    let lines = List.map items (fun (k,v) -> tr [ td [ pcdata k ] ; td [ pcdata v ] ]) in
    match lines with
    | [] -> assert false
    | h :: t -> table ~a:[a_class ["table"]] h t

  let multicolumn_ul items =
    let open Html5.M in
    let items = List.map items ~f:(fun item -> li ~a:[a_style "float:left;width:10em"] [item]) in
    div ~a:[a_style "margin-bottom:1em"] [
      ul ~a:[a_style "width:30em"] items ;
      br ~a:[a_style "clear:left"] () ;
    ]

  let lsnd = List.map ~f:snd

  let k = Html5.M.pcdata

  let html_page page_title contents =
    let open Html5.M in
    let head =
      head (title (pcdata page_title)) [
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
        script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
        script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
      ] in
    html head (body [ div ~a:[a_class ["container"]]  contents ])

  let link_of_path text path =
    Html5.M.(a ~a:[a_href (String.concat ~sep:"/" path) ] [ pcdata text ])


  let tabs contents =
    let open Html5.M in
    let tab_toggle = a_user_data "toggle" "tab" in
    [
      ul ~a:[a_class ["nav";"nav-tabs"]] (
        match contents with
        | [] -> []
        | (id, label, _) :: t ->
          let first_item = li ~a:[a_class ["active"]] [ a ~a:[a_href ("#" ^ id) ; tab_toggle] [k label]] in
          let other_items = List.map t ~f:(fun (id, label, _) ->
              li [a ~a:[a_href ("#" ^ id) ; tab_toggle] [k label]]
            )
          in
          first_item :: other_items
      ) ;
      div ~a:[a_class ["tab-content"]] (
        match contents with
        | [] -> []
        | (id, _, contents) :: t ->
          let first_div = div ~a:[a_id id ; a_class ["tab-pane";"fade";"in";"active"]] contents in
          let other_divs = List.map t ~f:(fun (id,_,contents) ->
              div ~a:[a_id id ; a_class ["tab-pane";"fade"]] contents
            )
          in
          first_div :: other_divs
      )
    ]



  let fastQC_reports =
    let open W.Short_read_sample in
    List.bind list (fun x ->
        List.mapi (fastQC_report x) ~f:(fun i report ->
            Document.raw
              (* ["quality_control" ; "FastQC" ; x#sample.sample_id ; string_of_int i ] *)
              report
          )
      )

  let bam_bai_of_short_reads_samples_with_reference =
    List.map W.DNA_seq_with_reference.list ~f:(fun x ->
      x,
     (* Document.raw
        [ "aligned_reads" ; (x # sample).sample_id ]*)
      Document.raw (W.DNA_seq_with_reference.aligned_reads_indexed_bam x)
    )

  let custom_track_link_of_bam_bai ucsc_genome sample_id bam_bai =
    let local_path = string_of_path (Document.web_path bam_bai) in
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
    let link = Html5.M.(a ~a:[a_href url] [ pcdata sample_id ]) in
    Lwt.return link

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
    List.concat collections
    |> List.filter ~f:(fun (s,_) -> filter s)
    |> Lwt_list.map_p (fun ((s,_) as e) -> link_of_sample_doc e >>= fun link -> Lwt.return (s, link))

    >>= fun links ->
    let header = thead [ tr [ td [ k "Model" ] ; td [ k "Condition" ] ; td [ k "Experiment" ] ; td [ k "Sample" ] ] ] in
    let lines = List.map links ~f:(fun (s,link) ->
      tr [
        td [ k s.sample_model ] ;
        td [ k s.sample_condition ] ;
        td [ k (string_of_experiment s.sample_exp) ] ;
        td [ link ] ;
      ]
    )
    in
    match lines with
    | [] -> assert false
    | h :: t -> Lwt.return (table ~a:[a_class ["table"]] ~thead:header h t)

  let filter_ucsc_samples = List.filter_map ~f:(fun (s,item) ->
    match (s # genomic_reference : W.Genome.t :> genome) with
    | `ucsc genome -> Some (s#sample, (genome, (s#sample).sample_id, item))
    | _ -> None
   )

  let index_custom_tracks_section =
    let open Html5.M in
    Lwt_list.map_p
      (fun (sample, (sample_id, genome, doc)) -> doc >>= fun d -> Lwt.return (sample, (sample_id, genome, d)))
      (filter_ucsc_samples bam_bai_of_short_reads_samples_with_reference)

    >>= fun ucsc_samples ->
    link_table
      (const true)
      (fun (sample, (sample_id, genome, item)) -> custom_track_link_of_bam_bai sample_id genome item)
      [ucsc_samples]

    >>= fun aligned_reads_link_table ->
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

  let sample_pages =
    let open Html5.M in
    List.map W.samples ~f:(fun s ->
        let page = html_page (sprintf "Sample :: %s" s.sample_id) [
            h3 [k "Infos"] ;
            keyval_table [ ("Type", "FIXME") ]
          ]
        in
        s, Document.page [ "sample" ; s.sample_id ; "index.html" ] page
      )

  let browse_by_sample_div =
    let open Html5.M in
    let link_of_sample (s,page_s) =
      page_s >>= fun page_s ->
      Lwt.return (Document.a page_s [ k s.sample_id ])
    in
    Lwt_list.map_p link_of_sample sample_pages >>= fun items ->
    Lwt.return (multicolumn_ul items)


  let browse_by_div =
    let open Html5.M in
    browse_by_sample_div >>= fun browse_by_sample_div ->
    let tabs = tabs [
        "browse-by-sample", "Samples", [ browse_by_sample_div ] ;
        "browse-by-condition", "Conditions", [ k"Under construction" ]
      ]
    in
    let div = div ((k "Browse by...") :: tabs) in
    Lwt.return div

  let index =
    let open Html5.M in
    index_custom_tracks_section >>= fun index_custom_tracks_section ->
    browse_by_div >>= fun browse_by_div ->
    html_page "Guizmin workflow" [
      h1 [b [k"Guizmin_workflow"]] ;
      hr () ;
      browse_by_div ;
      (* index_quality_control_section () ; *)
      index_custom_tracks_section ;
    ]
    |> Document.page ["index.html"]
    |> Lwt.return
end

let make_website (module W : Guizmin.Unrolled_workflow.S) workflow_output ~output_dir ~webroot =
  let module P = struct
    let workflow_output = workflow_output
    let output_dir = output_dir
    let webroot = webroot
  end in
  let module WWW = Make_website(W)(P) in
  mkdir_p output_dir ;
  WWW.index >|= ignore


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
