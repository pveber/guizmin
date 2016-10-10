open Core.Std
open Tyxml_html
open Experiment_description
open Guizmin_misc.Infix
open Bistro.Std
open Bistro_bioinfo.Std

module W = Website
open Website.Infix

let read_table fn =
  In_channel.read_lines fn
  |> List.map ~f:(String.split ~on:'\t')

module type Params = sig
  val dest : string
  val webroot : string
end

type path = string list
type 'a output = 'a Bistro_app.path

module type Service = sig
  type param
  type data

  val domain : param list
  val path : param -> path
  val data : param -> data Website.build
  val render : data -> Tyxml_html.doc
end

module Make(Params : Params)(P : Unrolled_workflow.S) = struct
  open Params

  (* === HTML HELPERS ===*)
  let k = pcdata

  let bb x = [ b [ k x ] ]

  let medskip = [ br () ; br () ; ]

  let opt x f = Option.value_map x ~default:[] ~f

  let keyval_table ?(style = "") items =
    let the_style = style in
    let lines = List.map items ~f:(fun (k,v) -> tr [ td k ; td v]) in
    table ~a:[a_class ["table"] ; a_style the_style] lines

  let keyval_table_opt ?style items =
    keyval_table ?style (List.filter_map items ~f:ident)

  let multicolumn_ul ?(n = 3) items =
    let items = List.map items ~f:(fun item -> li [item]) in
    div ~a:[a_style "column-count:3;-webkit-column-count:3;-moz-column-count:3;"] [
      ul items ;
    ]

  let html_base =
    if String.is_suffix webroot ~suffix:"/" then webroot
    else webroot ^ "/"

  let html page_title contents =
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
    a ~a:[a_href (String.concat ~sep:"/" path) ] [ pcdata text ]


  let tabs contents =
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
    let contents = List.concat paragraphs in
    if contents = [] then []
    else
      (h2 ?a [k title]) :: intro @ contents

  let subsection ?a ?(intro = []) title paragraphs =
    let contents = List.concat paragraphs in
    if contents = [] then
      []
    else
      (h4 ?a [k title]) :: intro @ contents


  module Model = struct
    open Option.Monad_infix

    type param = model
    type data = {
      model : model ;
      transcriptome : transcriptome ;
    }
    and transcriptome = {
      deseq2 : deseq2 option
    }
    and deseq2 = {
      comparison_summary : Deseq2.table output ;
      comparisons : ((string * string * string) * Deseq2.table output) list ;
      sample_pca_link : svg W.link ;
    }

    let deseq2 o =
      W.pure (fun comparison_summary comparisons sample_pca_link -> { comparison_summary ; comparisons ; sample_pca_link })
      $ W.pureW o#comparison_summary
      $ W.list (
        List.map o#comparisons ~f:(fun (cond, w) -> W.pure (fun b -> cond, b) $ W.pureW w)
      )
      $ W.pureP (W.data_page o#sample_pca)

    let ( $? ) x f =
      (match x with
          | None -> W.pure None
          | Some o -> W.pure (fun x -> Some x) $ f o)

    let transcriptome m =
      W.pure (fun deseq2 -> { deseq2 })
      $ (P.Transcriptome.deseq2 m $? deseq2)

    let data model =
      W.pure (fun transcriptome -> { model ; transcriptome })
      $ transcriptome model

    let comparison_table (Bistro_app.Path path : Deseq2.table output) =
      match read_table path with
      | [] -> assert false
      | header :: data ->
        let data' =
          data
          |> List.filter ~f:(List.last % Option.value_map ~default:false ~f:(( <> ) "NA"))
          |> List.filter ~f:(Fn.flip List.nth 2 % Option.value_map ~default:false ~f:(( <> ) "NA"))
          |> List.sort ~cmp:(fun x y ->
              let f x = Option.map (List.nth x 2) ~f:(Float.of_string % Float.abs) in
              compare (f y) (f x)
            )
        in
        [
          table
            ~a:[a_class ["table"]]
            ~thead:(thead [tr (List.map header ~f:(fun x -> td [ pcdata x ]))])
            (List.map data' ~f:(List.map ~f:(fun x -> td [ pcdata x ]) % tr))
        ]

    let comparison_summary_table (Bistro_app.Path path : Deseq2.table output) =
      match read_table path with
      | [] -> assert false
      | header :: data ->
        [
          table
            ~a:[a_class ["table"]]
            ~thead:(thead [tr (List.map header ~f:(fun x -> td [ pcdata x ]))])
            (List.map data ~f:(List.map ~f:(fun x -> td [ pcdata x ]) % tr))
        ]

    let comparison_tabs comparisons =
      let tables = List.map comparisons ~f:(snd % comparison_table) in
      List.map2_exn comparisons tables ~f:(fun ((name, l1, l2), _) table ->
          sprintf "deseq2-comp-%s-%s-%s" name l1 l2,
          sprintf "%s: %s vs %s" name l1 l2,
          table
        )
      |> tabs

    let mRNA_diff_expr_subsection d =
      let comparison_summary_table = comparison_summary_table d.comparison_summary in
      let comparison_tabs = comparison_tabs d.comparisons in
      subsection
        ~a:[a_id "mRNA-differential-expression-with-DESeq2"] "Differential expression with DESeq2"
        [ comparison_summary_table ; comparison_tabs ]

    let mRNA_section d =
      opt d.transcriptome.deseq2 @@ fun deseq2 ->
      section ~a:[a_id "mRNA"] "mRNA levels" [
        medskip ;
        [ p [ strong [ k"Sample overview " ] ; ] ] ;
        (* [ img ~a:[a_style "width:40%"] ~src:(W.href clustering) ~alt:"" () ] ; *)
        [ img ~a:[a_style "width:40%"] ~src:(W.href deseq2.sample_pca_link) ~alt:"" () ] ;
        mRNA_diff_expr_subsection deseq2 ;
      ]

    let title m = [
      h1 [b [k "Model " ; k m.model_id ]] ;
      hr () ;
    ]

    let sections d =
      List.concat [
        mRNA_section d ;
      ]

    let render ({ model = m } as d) =
      let page_title = sprintf "Model :: %s" m.model_id in
      let contents = List.concat [ title m ; medskip ; medskip ; sections d ] in
      html page_title contents
  end

  (* module Model_transcriptome_deseq2_comparison_summary = struct *)
  (*   type param = model *)
  (*   type data = Deseq2.table output *)

  (*   let domain = List.filter W.Model.list ~f:(fun m -> W.Transcriptome.deseq2 m <> None) *)
  (*   let path m = [ m.model_id ; "transcriptome" ; "deseq2" ; "comparison_summary"] *)
  (*   let data_term m  *)
  (* end *)



    (* let list = assoc W.Model.list ~f:(fun m -> *)
    (*     make m >>= *)
    (*     html_page [ "model" ; m.model_id ^ ".html" ] *)
    (*   ) *)

  (*   let list = [] *)
  (* end *)

  (* module Index = struct *)
  (*   let path = [ "index.html" ] *)

  (*   (\* let browse_by_model_div = *\) *)
  (*   (\*   Model_page.list *\) *)
  (*   (\*   |> List.map ~f:(fun (m, page_m) -> W.a page_m [ k m.model_id ]) *\) *)
  (*   (\*   |> multicolumn_ul *\) *)

  (*   let browse_by_div = *)
  (*     (\* browse_by_sample_div >>= fun browse_by_sample_div -> *\) *)
  (*     let tabs = tabs [ *)
  (*         (\* "browse-by-sample", "Sample", [ browse_by_sample_div ] ; *\) *)
  (*         (\* "browse-by-model",  "Model",  [ browse_by_model_div ] ; *\) *)
  (*       ] *)
  (*     in *)
  (*     div ((k "Browse by...") :: tabs) *)

  (*   let contents = *)
  (*     html "Guizmin workflow" [ *)
  (*       h1 [b [k"Project " ; i [k P.project_name]]] ; *)
  (*       hr () ; *)
  (*       br () ; *)
  (*       br () ; *)
  (*       browse_by_div ; *)
  (*     ] *)

  (*   let page = W.html_page path contents *)
  (* end *)

  (* let make () = Index.page *)
end

(* let generate ~dest ~webroot (module Project : Unrolled_workflow.S) = *)
(*   let module Params = struct *)
(*     let dest = dest *)
(*     let webroot = webroot *)
(*   end *)
(*   in *)
(*   let module WWW = Make(Params)(Project) in *)
(*   W.generate ~dest (WWW.make ()) *)
