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
  module WWW = Website.Make(struct end)

  (* let workflow_output' w = workflow_output (Bistro_workflow.u w) *)


  let string_of_experiment = function
    | `whole_cell_extract -> "WCE"
    | `TF_ChIP tf -> Printf.sprintf "ChIP-seq (%s)" tf
    | `FAIRE -> "FAIRE"
    | `mRNA -> "mRNA"

  let string_of_sample_type = function
    | `short_reads _ -> sprintf "Short reads"

  let string_of_genome = function
    | `ucsc g -> Ucsc_gb.string_of_genome g
    | `fasta _ -> "custom genome"

  let string_of_model m =
    let details =
      Option.value_map m.model_genome ~default:"" ~f:(fun g -> sprintf " (%s)" (string_of_genome g))
    in
    sprintf "%s%s" m.model_id details



  (* === HTML HELPERS ===*)

  let keyval_table ?(style = "") items =
    let the_style = style in
    let open Html5.M in
    let lines = List.map items (fun (k,v) -> tr [ td k ; td v]) in
    match lines with
    | [] -> assert false
    | h :: t -> table ~a:[a_class ["table"] ; a_style the_style] h t

  let lsnd = List.map ~f:snd

  let k = Html5.M.pcdata

  let keyval_table_opt ?style items =
    keyval_table ?style (List.filter_map items ~f:ident)

  let multicolumn_ul items =
    let open Html5.M in
    let items = List.map items ~f:(fun item -> li ~a:[a_style "float:left;width:10em"] [item]) in
    div ~a:[a_style "margin-bottom:1em"] [
      ul ~a:[a_style "width:30em"] items ;
      br ~a:[a_style "clear:left"] () ;
    ]

  let html_base =
    if String.is_suffix webroot ~suffix:"/" then webroot
    else webroot ^ "/"

  let html_page page_title contents =
    let open Html5.M in
    let head =
      head (title (pcdata page_title)) [
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
        link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
        script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
        script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
        base ~a:[a_href html_base] () ;
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








  let ucsc_filter xs = List.filter_map xs ~f:(fun (s, v) ->
      match s # reference_genome # repr with
      | `ucsc g -> Some (s, (g, v))
      | _ -> None

    )

  let indexed_bams =
    List.map W.mappable_short_read_samples ~f:(fun x ->
        x,
        WWW.raw
          ~path:[ "aligned_reads" ; x # id ]
          x#aligned_reads_indexed_bam
      )

  let fastQC_reports =
    List.map W.short_read_samples ~f:(fun x ->
        x,
        WWW.raw
          ~path:[ "quality_control" ; "FastQC" ; x#id ]
          (FastQC.html_report x#fastQC_report)
      )

  let fastQC_per_base_sequence_content =
    List.map W.short_read_samples ~f:(fun x ->
        x,
        WWW.raw
          (FastQC.per_base_sequence_content x#fastQC_report)
      )

  let fastQC_per_base_quality =
    List.map W.short_read_samples ~f:(fun x ->
        x,
        WWW.raw
          (FastQC.per_base_quality x#fastQC_report)
      )


  let custom_track_link_of_bam_bai x genome bam_bai elt =
    let local_path = string_of_path (WWW.path bam_bai) in
    let name = x#id ^ " aligned_reads" in
    let opts = [
      `track_type "bam" ;
      `bigDataUrl (webroot ^ "/" ^ local_path ^ "/reads.bam") ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Gzt.Ucsc_gb.CustomTrack.url genome opts in
    [ Html5.M.(a ~a:[a_href url] elt) ]

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

  let link_table filter link_of_sample_doc collections =
    let open Html5.M in
    let links =
      List.concat collections
      |> List.filter ~f:(fun (s,_,_) -> filter s)
      |> List.map ~f:(fun ((s,_,_) as e) -> s, link_of_sample_doc e)
    in
    let header = thead [ tr [ td [ k "Model" ] ; td [ k "Condition" ] ; td [ k "Experiment" ] ; td [ k "Sample" ] ] ] in
    let lines = List.map links ~f:(fun (s,link) ->
      tr [
        td [ k s#repr.sample_model ] ;
        td [ k s#repr.sample_condition ] ;
        td [ k (string_of_experiment s#repr.sample_exp) ] ;
        td [ link ] ;
      ]
    )
    in
    match lines with
    | [] -> assert false
    | h :: t -> table ~a:[a_class ["table"]] ~thead:header h t

  (* let index_custom_tracks_section = *)
  (*   let open Html5.M in *)
  (*   let aligned_reads_link_table = *)
  (*     indexed_bam *)
  (*     |> ucsc_filter *)
  (*     |> List.map ~f:(fun (sample, (genome,doc)) -> (sample, genome, doc)) *)
  (*     |> (fun ucsc_samples -> *)
  (*         link_table *)
  (*           (const true) *)
  (*           (fun (sample, genome, doc) -> custom_track_link_of_bam_bai sample genome doc [k sample#id]) *)
  (*           [ucsc_samples]) *)
  (*   (\* let signal_link_table = *\) *)
  (*   (\*   link_table *\) *)
  (*   (\* (const true) *\) *)
  (*   (\* (custom_track_link_of_bigwig_item webroot) *\) *)
  (*   (\* [ chipseq_bigwig ; wceseq_bigwig ; faireseq_bigwig (\\* ; rnaseq_bigwig *\\) ] *\) *)
  (*   in *)
  (*   div [ *)
  (*     h2 ~a:[a_id "custom-tracks"] [k "UCSC Genome Browser custom tracks"] ; *)
  (*     p [k "The datasets can be visualized on the " ; *)
  (*        a ~a:[a_href "http://genome.ucsc.edu/cgi-bin/hgTracks"] [k"UCSC Genome Browser"] ; *)
  (*        k ". To achieve this, simply click on the link corresponding to the sample you want to visualize." ; *)
  (*        k " In order to keep a particular combination of custom tracks on the browser, consider using " ; *)
  (*        a ~a:[a_href "http://genome.ucsc.edu/goldenPath/help/hgSessionHelp.html"] [k"sessions"] ; *)
  (*        k"." *)
  (*       ] ; *)
  (*     h3 ~a:[a_id "custom-tracks-aligned-reads"] [k "Aligned reads"] ; *)
  (*     p [k "These tracks display the raw alignments of reads from HTS samples. "] ; *)
  (*     aligned_reads_link_table ; *)
  (*     (\* h3 ~a:[a_id "custom-tracks-signal"] [k "Signal"] ; *\) *)
  (*     (\* p [k "These tracks display the raw signal from HTS samples. "] ; *\) *)
  (*     (\* signal_link_table ; *\) *)
  (*   ] *)


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

  module Sample_page = struct
    open Html5.M

    type fragment = [ Html5_types.flow5 ] elt list

    let bb x = [ b [ k x ] ]

    let ( $ ) assoc x = List.find_exn assoc ~f:(fun (y,_) -> x#id = y#id) |> snd

    let upcast xs x = List.find xs ~f:(fun y -> x # id = y # id)

    let ( >>+ ) l = function
      | Some x -> l @ [ x ]
      | None -> l

    let ( >>@ ) l = function
      | Some x -> l @ x
      | None -> l

    class base s = object (self)
      method sample = (s :> Unrolled_workflow.sample)
      method overview : fragment Lwt.t = Lwt.return [
          h1 [b [k "Sample " ; k s#id ]] ;
          hr () ;
          br () ;
          br () ;
          h2 [k "Overview"] ;
          br () ;
          keyval_table ~style:"width:50%" [
            bb"Type", [ k (string_of_sample_type s#_type) ] ;
            bb"Experiment", [ k (string_of_experiment s#repr.sample_exp) ] ;
            bb"Model", [ k s#repr.sample_model ] ;
            bb"Condition", [ k s#repr.sample_condition ] ;
          ] ;
        ]
      method paragraphs : fragment list Lwt.t =
        self # overview >|= fun o -> [ o ]

      method make () : Website.html_elt Lwt.t =
        self#paragraphs >>= fun pgs ->
        let pgs = List.intersperse pgs [ br () ; br () ] in
        Lwt.return (html_page (sprintf "Sample :: %s" s#id) (List.concat pgs))
    end
    let base s = new base s

    class short_read s = object (self)
      inherit base s as super
      method quality_check : fragment Lwt.t = Lwt.return [
          h2 [k "Sequencing quality check"] ;
          h3 [k "FastqQC report"] ;
          ul [
            li [
              k "Snapshot:" ;
              br () ;
              img ~a:[a_style "width:40% ; margin: 0 10%"] ~src:(WWW.href (fastQC_per_base_quality $ s)) ~alt:"" () ;
              img ~a:[a_style "width:40%"] ~src:(WWW.href (fastQC_per_base_sequence_content $ s)) ~alt:"" () ;
              br () ;
            ] ;
            li [ WWW.a (fastQC_reports $ s) [k "Access to the full report"] ] ;
            (* FIXME li [ link to FASTQC website ] *)
          ] ;
             ]
      method paragraphs =
        super # paragraphs >>= fun pgs ->
        self # quality_check >>= fun qc ->
        Lwt.return (pgs @ [ qc ])
    end
    let short_read s =
      (new short_read s :> base)

    class mappable_short_read s = object
      inherit short_read s as super
    end

    class mappable_short_read_ucsc s genome = object (self)
      inherit mappable_short_read s as super

      method aligned_bam_custom_track_link : fragment =
        let bam = indexed_bams $ s in
        custom_track_link_of_bam_bai s genome bam [k "Aligned reads" ]

      method custom_track_links : fragment list = [
        self#aligned_bam_custom_track_link ;
      ]

      method custom_tracks : fragment = [
        h2 ~a:[a_id "custom-tracks"] [k "UCSC Genome Browser custom tracks"] ;
        p [k "The datasets can be visualized on the " ;
           a ~a:[a_href "http://genome.ucsc.edu/cgi-bin/hgTracks"] [k"UCSC Genome Browser"] ;
           k ". To achieve this, simply click on the link corresponding to the sample you want to visualize." ;
           k " In order to keep a particular combination of custom tracks on the browser, consider using " ;
           a ~a:[a_href "http://genome.ucsc.edu/goldenPath/help/hgSessionHelp.html"] [k"sessions"] ;
           k"."
          ] ;
        ul (List.map self#custom_track_links ~f:li) ;
      ]

      method! paragraphs =
        super#paragraphs >>= fun pgs ->
        Lwt.return (pgs @  [ self # custom_tracks ] )
    end

    let mappable_short_read s =
      match s # reference_genome # repr with
      | `ucsc g -> (new mappable_short_read_ucsc s g :> base)
      | `fasta _ -> (new mappable_short_read s :> base)

    let contents = function
      | `TF_ChIP_seq s -> mappable_short_read s
      | `FAIRE_seq s -> mappable_short_read s
      | `WCE_seq s -> mappable_short_read s
      | `mRNA_seq s -> short_read s
      | `Short_read_sample s -> short_read s

    let list = List.map W.samples ~f:(fun s ->
        let obj = contents (W.any_sample s) in
        obj#sample,
        WWW.html_page
          [ "sample" ; obj#sample#id ; "index.html" ]
          ~f:obj#make
          ()
      )
  end

  let browse_by_sample_div =
    let open Html5.M in
    Sample_page.list
    |> List.map ~f:(fun (s,page_s) -> WWW.a page_s [ k s#id ])
    |> multicolumn_ul


  let browse_by_div =
    let open Html5.M in
    let tabs = tabs [
        "browse-by-sample", "Samples", [ browse_by_sample_div ] ;
        "browse-by-condition", "Conditions", [ k"Under construction" ]
      ]
    in
    div ((k "Browse by...") :: tabs)

  let index =
    let open Html5.M in
    let contents () =
      html_page "Guizmin workflow" [
        h1 [b [k"Project " ; i [k W.project_name]]] ;
        hr () ;
        br () ;
        br () ;
        browse_by_div ;
        (* index_quality_control_section () ; *)
        (* index_custom_tracks_section ; *)
      ]
    in
    WWW.html_page ["index.html"] ~f:(fun () -> Lwt.return (contents ())) ()
end

let make_website (module W : Guizmin.Unrolled_workflow.S) workflow_output ~output_dir ~webroot =
  let module P = struct
    let workflow_output = workflow_output
    let output_dir = output_dir
    let webroot = webroot
  end in
  let module WWW = Make_website(W)(P) in
  mkdir_p output_dir ;
  WWW.WWW.generate ~workflow_output ~output_dir


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
