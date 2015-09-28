open Core.Std
open Bistro_std
open Common
open Lwt_infix
open Misc.Infix

let string_of_path l = String.concat ~sep:"/" l

let read_table fn =
  Lwt.return (
    `Ok (
      In_channel.read_lines fn
      |> List.map ~f:(String.split ~on:'\t')
    )
  )


type 'a fragment = 'a Html5.M.elt list

type 'a result = [ `Ok of 'a | `Error of (Bistro.Workflow.u * string) list ] Lwt.t

module Result :
sig
  type 'a t = 'a result

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>=? ) : 'a option t -> ('a -> 'b option t) -> 'b option t
  val ( >>|? ) : 'a option t -> ('a -> 'b) -> 'b option t

  val ( >=? ) : 'a option -> ('a -> 'b option t) -> 'b option t
  val ( >|? ) : 'a option -> ('a -> 'b) -> 'b option t
  val merge : 'a t list -> 'a list t
  val merge_concat : 'a list t list -> 'a list t
end
=
struct
  type 'a t = [ `Ok of 'a | `Error of (Bistro.Workflow.u * string) list ] Lwt.t

  let return x = Lwt.return (`Ok x)

  let ( >=? ) x f = match x with
    | Some x' -> f x'
    | None -> return None

  let ( >|? ) x f = match x with
    | Some x' -> return (Some (f x'))
    | None -> return None

  let ( >>= ) x f = x >>= function
    | `Ok x -> f x
    | `Error e -> Lwt.return (`Error e)

  let ( >>=? ) x f = x >>= function
    | Some x -> f x
    | None -> return None

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( >>|? ) x f = x >>= function
    | Some x -> return (Some (f x))
    | None -> return None

  let merge xs =
    Lwt.bind
      (Lwt_list.map_p ident xs)
      (fun xs ->
         List.fold_left xs ~init:(`Ok []) ~f:(fun status res ->
             match status, res with
             | `Ok xs, `Ok ys -> `Ok (ys :: xs)
             | `Error es, `Ok _ -> `Error es
             | `Ok _, `Error es -> `Error es
             | `Error es, `Error es' -> `Error (es @ es')
           )
         |> (function
             | `Ok xs -> `Ok (List.rev xs)
             | `Error xs -> `Error xs)
         |> Lwt.return)

  let merge_concat xs =
    Lwt.bind
      (Lwt_list.map_p ident xs)
      (fun xs ->
         List.fold_left xs ~init:(`Ok []) ~f:(fun status res ->
             match status, res with
             | `Ok xs, `Ok ys -> `Ok (xs @ ys)
             | `Error es, `Ok _ -> `Error es
             | `Ok _, `Error es -> `Error es
             | `Error es, `Error es' -> `Error (es @ es')
           )
         |> Lwt.return)
end

module type Params = sig
  val workflow_output : Bistro.Workflow.u -> [ `Ok of string
                                             | `Error of (Bistro.Workflow.u * string) list] Lwt.t
  val output_dir : string
  val webroot : string
end


module Make_website(W : Guizmin.Unrolled_workflow.S)(P : Params) = struct
  open P
  open Guizmin
  open Experiment_description
  open Html5.M
  open Result

  let assoc xs ~f = List.map xs ~f:(fun x -> x, f x)

  let assoc_map_p xs ~f =
    let f x = snd x >>= fun y -> f (fst x) y in
    Lwt_list.map_p f xs

  let ( $ ) xs x =
    match List.Assoc.find xs x with
    | Some x -> x
    | None -> return None

  let workflow_output' x : string result =
    workflow_output (x : _ Bistro.Workflow.t :> Bistro.Workflow.u)

  (* WEBSITE GENERATION *)
  module WWW = Guizmin_repo.Make(struct let build x = workflow_output x end)

  let string_of_experiment = function
    | `whole_cell_extract -> "WCE"
    | `TF_ChIP tf -> Printf.sprintf "ChIP (%s)" tf
    | `EM_ChIP mark -> Printf.sprintf "ChIP (%s)" mark
    | `FAIRE -> "FAIRE"
    | `mRNA -> "mRNA"

  let string_of_sample_data = function
    | `short_read_data _ -> sprintf "Short reads"

  let string_of_genome = function
    | `ucsc g -> Ucsc_gb.string_of_genome g
    | `fasta _ -> "custom genome"

  let string_of_model m =
    let details =
      Option.value_map m.model_genome ~default:"" ~f:(fun g -> sprintf " (%s)" (string_of_genome g))
    in
    sprintf "%s%s" m.model_id details

  let string_of_condition c =
    List.map c ~f:(fun (fn, fv) -> sprintf "%s=%s" fn.factor_name fv)
    |> String.concat ~sep:","
    |> sprintf "(%s)"



  (* === HTML HELPERS ===*)
  let k = pcdata

  let bb x = [ b [ k x ] ]

  let medskip = [ br () ; br () ; ]

  let keyval_table ?(style = "") items =
    let the_style = style in
    let open Html5.M in
    let lines = List.map items (fun (k,v) -> tr [ td k ; td v]) in
    table ~a:[a_class ["table"] ; a_style the_style] lines

  let keyval_table_opt ?style items =
    keyval_table ?style (List.filter_map items ~f:ident)

  let multicolumn_ul ?(n = 3) items =
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

  let section ?a ?(intro = []) title paragraphs =
    let contents =
      paragraphs
      |> List.filter_map ~f:ident
      |> List.concat
    in
    if contents = [] then []
    else
      (h2 ?a [k title]) :: intro @ contents

  let subsection ?a ?(intro = []) title paragraphs =
    let contents =
      paragraphs
      |> List.filter_map ~f:ident
      |> List.concat
    in
    if contents = [] then
      []
    else
      (h4 ?a [k title]) :: intro @ contents




  (* PAGES CORRESPONDING TO DIRECT WORKFLOW OUTPUT *)

  let mapped_reads_indexed = assoc W.Sample.list ~f:(fun s ->
      W.Sample.mapped_reads_indexed s >|? fun bam ->
      WWW.file_page
        ~path:[ "sample" ; "mapped_reads" ; s.sample_id ]
        bam
    )

  let fastQC_report_page = assoc W.Sample.list ~f:(fun s ->
      W.Sample.fastQC_report s >|? function
      | `single_end report ->
        let page =
          WWW.file_page
            ~path:[ "sample" ; "quality_control" ; "FastQC" ; s.sample_id ]
            (FastQC.html_report report) in
        let per_base_sequence_content =
          WWW.file_page (FastQC.per_base_sequence_content report) in
        let per_base_quality =
          WWW.file_page (FastQC.per_base_quality report) in
        `single_end (page, per_base_sequence_content, per_base_quality)

      | `paired_end (report_1, report_2) ->
        let page_1 = WWW.file_page ~path:[ "quality_control" ; "FastQC" ; s.sample_id ^ "_1" ] report_1 in
        let page_2 = WWW.file_page ~path:[ "quality_control" ; "FastQC" ; s.sample_id ^ "_2" ] report_2 in
        let per_base_sequence_content_1 = WWW.file_page (FastQC.per_base_sequence_content report_1) in
        let per_base_sequence_content_2 = WWW.file_page (FastQC.per_base_sequence_content report_2) in
        let per_base_quality_1 = WWW.file_page (FastQC.per_base_quality report_1) in
        let per_base_quality_2 = WWW.file_page (FastQC.per_base_quality report_2) in
        `paired_end ((page_1, per_base_sequence_content_1, per_base_quality_1),
                     (page_2, per_base_sequence_content_2, per_base_quality_2))
    )

  let signal_page = assoc W.Sample.list ~f:(fun s ->
      W.Sample.signal s >|?
      WWW.file_page ~path:[ "sample" ; "signal" ; s.sample_id ^ ".bw" ]
    )

  let called_peaks = assoc W.Sample.list ~f:(fun s ->
      W.Sample.peak_calling s >|?
      WWW.file_page ~path:[ "sample" ; "called_peaks" ; s.sample_id ^ ".bed" ]
    )

  let macs2_peaks = assoc W.Sample.list ~f:(fun s ->
      W.Sample.macs2_peak_calling s >|? fun x ->
      WWW.file_page (Macs2.peaks_xls x)
    )

  let deseq2_sample_clustering = assoc W.Model.list ~f:(fun m ->
      W.Transcriptome.deseq2 m >|? fun deseq ->
      WWW.file_page deseq#sample_clustering
    )

  let deseq2_comparison_summary = assoc W.Model.list ~f:(fun m ->
      W.Transcriptome.deseq2 m >|? fun deseq ->
      WWW.file_page
        ~path:[ "model" ; m.model_id ; "deseq2" ; "comparison_summary.tsv" ]
        deseq#comparison_summary
    )

  let deseq2_sample_pca = assoc W.Model.list ~f:(fun m ->
      W.Transcriptome.deseq2 m >|? fun deseq ->
      WWW.file_page deseq#sample_pca
    )

  let deseq2_sample_pca = assoc W.Model.list ~f:(fun m ->
      W.Transcriptome.deseq2 m >|? fun deseq ->
      List.map deseq#comparisons ~f:(fun (id, comp) ->
          id, WWW.file_page comp
        )
    )

  let called_peaks_bb : (W.Sample.t * WWW.page option result) list = assoc W.Sample.list ~f:(fun s ->
      W.Sample.ucsc_genome s >=? fun org ->
      W.Sample.peak_calling s >=? fun bed ->
      workflow_output' bed >>| fun bed_path ->
      if file_is_empty bed_path then None
      else
        let page =
          WWW.file_page
            ~path:[ "sample" ; "called_peaks" ; s.sample_id ^ ".bb" ]
            (Ucsc_gb.bedToBigBed_failsafe org (`bed3 (Bed.keep3 bed)))
            (* macs2 has numerous non standard fields, which make it
               incompatible with bedToBigBed as is. That's why we keep
               only 3 fields when building the bigBed (field 4 has no
               interest here and field 5 contains number potentially
               greater than 1000). *)
        in
        Some page
    )

  let htseq_counts = assoc W.Sample.list ~f:(fun s ->
      W.Sample.read_counts_per_gene s >|? WWW.file_page
    )

  let custom_track_link_of_bam_bai x genome bam_bai elt =
    let local_path = string_of_path (WWW.path bam_bai) in
    let name = x.sample_id ^ " mapped reads" in
    let opts = [
      `track_type "bam" ;
      `bigDataUrl (webroot ^ "/" ^ local_path ^ "/reads.bam") ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Ucsc_utils.CustomTrack.url genome opts in
    [ a ~a:[a_href url] elt ]

  let custom_track_link_of_bigwig x genome bigwig elt =
    let local_path = string_of_path (WWW.path bigwig) in
    let name = x.sample_id ^ " signal" in
    let opts = [
      `track_type "bigWig" ;
      `bigDataUrl (webroot ^ "/" ^ local_path) ;
      `color (0,0,255) ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Ucsc_utils.CustomTrack.url genome opts in
    [ a ~a:[a_href url] elt ]

  let custom_track_link_of_bigBed x genome bigBed objects elt =
    let local_path = string_of_path (WWW.path bigBed) in
    let name = x.sample_id ^ " " ^ objects in
    let opts = [
      `track_type "bigBed" ;
      `bigDataUrl (webroot ^ "/" ^ local_path) ;
      `color (0,0,255) ;
      `visibility `dense ;
      `name name ;
      `description name ;
    ]
    in
    let url = Ucsc_utils.CustomTrack.url genome opts in
    [ a ~a:[a_href url] elt ]

(*   (\* let index_custom_tracks_section = *\) *)
(*   (\*   let open Html5.M in *\) *)
(*   (\*   let aligned_reads_link_table = *\) *)
(*   (\*     indexed_bam *\) *)
(*   (\*     |> ucsc_filter *\) *)
(*   (\*     |> List.map ~f:(fun (sample, (genome,doc)) -> (sample, genome, doc)) *\) *)
(*   (\*     |> (fun ucsc_samples -> *\) *)
(*   (\*         link_table *\) *)
(*   (\*           (const true) *\) *)
(*   (\*           (fun (sample, genome, doc) -> custom_track_link_of_bam_bai sample genome doc [k sample#id]) *\) *)
(*   (\*           [ucsc_samples]) *\) *)
(*   (\*   (\\* let signal_link_table = *\\) *\) *)
(*   (\*   (\\*   link_table *\\) *\) *)
(*   (\*   (\\* (const true) *\\) *\) *)
(*   (\*   (\\* (custom_track_link_of_bigwig_item webroot) *\\) *\) *)
(*   (\*   (\\* [ chipseq_bigwig ; wceseq_bigwig ; faireseq_bigwig (\\\* ; rnaseq_bigwig *\\\) ] *\\) *\) *)
(*   (\*   in *\) *)
(*   (\*   div [ *\) *)
(*   (\*     h2 ~a:[a_id "custom-tracks"] [k "UCSC Genome Browser custom tracks"] ; *\) *)
(*   (\*     p [k "The datasets can be visualized on the " ; *\) *)
(*   (\*        a ~a:[a_href "http://genome.ucsc.edu/cgi-bin/hgTracks"] [k"UCSC Genome Browser"] ; *\) *)
(*   (\*        k ". To achieve this, simply click on the link corresponding to the sample you want to visualize." ; *\) *)
(*   (\*        k " In order to keep a particular combination of custom tracks on the browser, consider using " ; *\) *)
(*   (\*        a ~a:[a_href "http://genome.ucsc.edu/goldenPath/help/hgSessionHelp.html"] [k"sessions"] ; *\) *)
(*   (\*        k"." *\) *)
(*   (\*       ] ; *\) *)
(*   (\*     h3 ~a:[a_id "custom-tracks-aligned-reads"] [k "Aligned reads"] ; *\) *)
(*   (\*     p [k "These tracks display the raw alignments of reads from HTS samples. "] ; *\) *)
(*   (\*     aligned_reads_link_table ; *\) *)
(*   (\*     (\\* h3 ~a:[a_id "custom-tracks-signal"] [k "Signal"] ; *\\) *\) *)
(*   (\*     (\\* p [k "These tracks display the raw signal from HTS samples. "] ; *\\) *\) *)
(*   (\*     (\\* signal_link_table ; *\\) *\) *)
(*   (\*   ] *\) *)

  module Sample_page = struct
    let fastQC_single_end_paragraph s (html, snapshot1, snapshot2) =
      ul [
        li [
          k "Snapshot:" ;
          br () ;
          img ~a:[a_style "width:40% ; margin: 0 10%"] ~src:(WWW.href snapshot1) ~alt:"" () ;
          img ~a:[a_style "width:40%"] ~src:(WWW.href snapshot2) ~alt:"" () ;
          br () ;
        ] ;
        li [ k "Check the " ; WWW.a html [k "full report"] ] ;
        li [ a ~a:[a_href "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/"] [ k "More information on FastQC" ] ] ;
      ]

    let fastQC_paired_end_paragraph s (html_1, snapshot1_1, snapshot2_1) (html_2, snapshot1_2, snapshot2_2) =
      ul [
        li [
          k "Snapshot (forward):" ;
          br () ;
          img ~a:[a_style "width:40% ; margin: 0 10%"] ~src:(WWW.href snapshot1_1) ~alt:"" () ;
          img ~a:[a_style "width:40%"] ~src:(WWW.href snapshot2_1) ~alt:"" () ;
          br () ;
          k "Snapshot (reverse):" ;
          br () ;
          img ~a:[a_style "width:40% ; margin: 0 10%"] ~src:(WWW.href snapshot1_2) ~alt:"" () ;
          img ~a:[a_style "width:40%"] ~src:(WWW.href snapshot2_2) ~alt:"" () ;
        ] ;
        li [ k "Check the full reports:" ; WWW.a html_1 [k "Forward"] ; WWW.a html_2 [k "Reverse"] ] ;
        li [ a ~a:[a_href "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/"] [ k "More information on FastQC" ] ] ;
      ]

    let fastQC_paragraph s =
      fastQC_report_page $ s >>|? fun fqc_rp ->
      let par = match fqc_rp with
        | `single_end report ->
          fastQC_single_end_paragraph s report
        | `paired_end (report_1, report_2) ->
          fastQC_paired_end_paragraph s report_1 report_2
      in
      [ h3 [ k "FastqQC report" ] ; par ]

    let sequencing_quality_check_section s =
      fastQC_paragraph s >>| fun fqc_par ->
      section "Sequencing quality check" [
        fqc_par ;
      ]

    let macs2_paragraph s =
      macs2_peaks $ s >>|? fun xls ->
      [
        h3 [ k "MACS2 output" ] ;
        ul [
          li [ WWW.a xls [ k "Called peaks" ] ]
        ] ;
      ]

    let peak_calling_section s =
      macs2_paragraph s >>| fun macs2_par ->
      section "Peak calling" [
        macs2_par ;
      ]

    let htseq_paragraph s =
      htseq_counts $ s >>|? fun tsv ->
      [
        h3 [ k "HTSeq" ] ;
        ul [
          li [ WWW.a tsv [ k "Counts per gene" ] ] ;
        ] ;
      ]

    let mrna_seq_section s =
      htseq_paragraph s >>| fun htseq_par ->
      section "Expression" [
         htseq_par ;
      ]

    let mapped_reads_indexed_custom_track_link s =
      W.Sample.ucsc_genome s >=? fun org ->
      mapped_reads_indexed $ s >>|? fun bam_bai ->
      custom_track_link_of_bam_bai s org bam_bai [k "Mapped reads" ]

    let signal_custom_track_link s =
      W.Sample.ucsc_genome s >=? fun org ->
      signal_page $ s >>|? fun bigWig ->
      custom_track_link_of_bigwig s org bigWig [k "Signal intensity" ]

    let called_peaks_custom_track_link s : _ option result =
      W.Sample.ucsc_genome s >=? fun org ->
      called_peaks_bb $ s >>|? fun bigBed ->
      custom_track_link_of_bigBed s org bigBed "called peaks" [k "Called peaks" ]

    let custom_track_links s =
      called_peaks_custom_track_link s >>= fun called_peaks_custom_track_link ->
      mapped_reads_indexed_custom_track_link s >>= fun mapped_reads_indexed_custom_track_link ->
      signal_custom_track_link s >>= fun signal_custom_track_link ->
      let links = List.filter_map ~f:ident [
        mapped_reads_indexed_custom_track_link ;
        signal_custom_track_link ;
        called_peaks_custom_track_link ;
        ]
      in
      return (if links = [] then None else Some links)

    let custom_tracks_link_list s =
      custom_track_links s >>|? fun links ->
      [ ul (List.map links ~f:li) ]

    let custom_tracks_section s =
      custom_tracks_link_list s >>| fun link_list ->
      let intro = [
        p [k "The datasets can be visualized on the " ;
           a ~a:[a_href "http://genome.ucsc.edu/cgi-bin/hgTracks"] [k"UCSC Genome Browser"] ;
           k ". To achieve this, simply click on the link corresponding to the sample you want to visualize." ;
           k " In order to keep a particular combination of custom tracks on the browser, consider using " ;
           a ~a:[a_href "http://genome.ucsc.edu/goldenPath/help/hgSessionHelp.html"] [k"sessions"] ;
           k"."
          ] ;
      ] in
      section ~a:[a_id "custom-tracks"] ~intro "UCSC Genome Browser custom tracks" [ link_list ]

    let title s = [
      h1 [b [k "Sample " ; k s.sample_id ]] ;
      hr () ;
    ]

    let overview s = [
      h2 [k "Overview"] ;
      br () ;
      keyval_table ~style:"width:50%" [
        bb"Type", [ k (string_of_sample_data s.sample_data) ] ;
        bb"Experiment", [ k (string_of_experiment s.sample_exp) ] ;
        bb"Model", [ k s.sample_model ] ;
        bb"Condition", [ k (string_of_condition (W.Sample.condition s)) ] ;
      ] ;
    ]

    let sections s =
      merge_concat [
        sequencing_quality_check_section s ;
        peak_calling_section s ;
        mrna_seq_section s ;
        custom_tracks_section s ;
      ]

    let make s : 'a result =
      let page_title = sprintf "Sample :: %s" s.sample_id in
      let open Lwt_infix in
      sections s >>| fun sections ->
      let contents = List.concat [ title s ; medskip ; overview s ; medskip ; sections ] in
      html_page page_title contents

    let list = assoc W.Sample.list ~f:(fun s ->
        make s >>|
        WWW.html_page [ "sample" ; s.sample_id ^ ".html" ]
      )
  end

  module Model_page = struct
    let comparison_summary u : _ fragment result =
      workflow_output' u >>= fun path ->
      read_table path >>| function
      | [] -> assert false
      | header :: data ->
        [
          table
            ~a:[a_class ["table"]]
            ~thead:(thead [tr (List.map header ~f:(fun x -> td [ pcdata x ]))])
            (List.map data ~f:(List.map ~f:(fun x -> td [ pcdata x ]) % tr))
        ]

    let mRNA_diff_expr_subsection m : _ fragment result =
      W.Transcriptome.deseq2 m >=? fun deseq2 ->
      comparison_summary deseq2#comparison_summary >>| Option.some >>| fun comparison_summary ->
      subsection ~a:[a_id "mRNA-differential-expression-with-DESeq2"] "Differential expression with DESeq2" [
        comparison_summary ;
      ]

    let mRNA_section m =
      mRNA_diff_expr_subsection m >>| fun diff_subsection ->
      deseq2_sample_clustering $ m >>|? fun clustering ->
      Some (
        section ~a:[a_id "mRNA"] "mRNA levels" [
          Some [ img ~a:[a_style "width:40%"] ~src:(WWW.href clustering) ~alt:"" () ] ;
          diff_subsection ;
        ]
      )

    let title m = [
      h1 [b [k "Model " ; k m.model_id ]] ;
      hr () ;
    ]

    let overview s = [
      h2 [k "Overview"] ;
      br () ;
      keyval_table ~style:"width:50%" [
        (* bb"Type", [ k (string_of_sample_data s.sample_data) ] ; *)
        (* bb"Experiment", [ k (string_of_experiment s.sample_exp) ] ; *)
        (* bb"Model", [ k s.sample_model ] ; *)
        (* bb"Condition", [ k (string_of_condition (W.Sample.condition s)) ] ; *)
      ] ;
    ]

    let sections m =
      merge_concat [
        mRNA_section m
      ]

    let make m =
      let page_title = sprintf "Model :: %s" m.model_id in
      sections m >>| fun sections ->
      let contents = List.concat [ title m ; medskip ; overview m ; medskip ; sections ] in
      html_page page_title contents

    let list = assoc W.Model.list ~f:(fun m ->
        make m >>|
        WWW.html_page [ "model" ; m.model_id ^ ".html" ]
      )
  end

  module Index = struct
    let browse_by_sample_div =
      let open Html5.M in
      Sample_page.list
      |> List.map ~f:(fun (s,page_s) -> page_s >>| fun page_s -> WWW.a page_s [ k s.sample_id ])
      |> merge
      >>| multicolumn_ul

    let browse_by_model_div =
      let open Html5.M in
      Model_page.list
      |> List.map ~f:(fun (m, page_m) -> page_m >>| fun page_m -> WWW.a page_m [ k m.model_id ])
      |> merge
      >>| multicolumn_ul

    let browse_by_div =
      let open Html5.M in
      browse_by_sample_div >>= fun browse_by_sample_div ->
      browse_by_model_div >>| fun browse_by_model_div ->
      let tabs = tabs [
          "browse-by-sample", "Sample", [ browse_by_sample_div ] ;
          "browse-by-model",  "Model",  [ browse_by_model_div ] ;
        ]
      in
      div ((k "Browse by...") :: tabs)

    let page =
      let open Html5.M in
      browse_by_div >>| fun browse_by_div ->
      let contents =
        html_page "Guizmin workflow" [
          h1 [b [k"Project " ; i [k W.project_name]]] ;
          hr () ;
          br () ;
          br () ;
          browse_by_div ;
        ]
      in
      WWW.html_page ["index.html"] contents
  end
end

let make_website (module W : Guizmin.Unrolled_workflow.S) workflow_output ~output_dir ~webroot =
  let module P = struct
    let workflow_output = workflow_output
    let output_dir = output_dir
    let webroot = webroot
  end in
  let module WWW = Make_website(W)(P) in
  mkdir_p output_dir ;
  WWW.WWW.generate output_dir


let check_errors descr =
  match Guizmin.Experiment_description.check descr with
  | [] -> ()
  | xs ->
    List.map xs ~f:Guizmin.Experiment_description.error_msg
    |> String.concat ~sep:", "
    |> failwith

open Bistro_engine

let main opts dopts ged_file output_dir webroot = Guizmin.(
  let description = Experiment_description.load ged_file in
  check_errors description ;
  let module W = (val Unroll_workflow.from_description description) in
  let db = Db.init_exn "_guizmin" in
  let scheduler = Scheduler.make ~np:dopts.np ~mem:(dopts.mem * 1024) db in
  let workflow_output u = Scheduler.build' scheduler u in
  make_website (module W) workflow_output ~output_dir ~webroot >>= function
  | `Ok () -> Lwt.return ()
  | `Error xs ->
    fprintf stderr "Some workflow(s) failed:\n" ;
    List.iter xs ~f:(fun (u, msg) ->
        fprintf stderr "\t%s\t%s\n" (Bistro.Workflow.id' u) msg
      ) ;
    List.iter xs ~f:(fun (u, _) -> Db.output_report db u stderr) ;
    Lwt.return ()
  )
  |> Lwt_unix.run
