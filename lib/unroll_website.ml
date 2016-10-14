open Core.Std
open Tyxml_html
open Experiment_description
open Guizmin_misc.Infix
open Bistro.Std
open Bistro_bioinfo.Std
open Website.Syntax

module W = Website
open Website.Infix

let read_table fn =
  In_channel.read_lines fn
  |> List.map ~f:(String.split ~on:'\t')

let assoc_make keys ~f = List.map keys ~f:(fun k -> k, f k)

let assoc_make' keys ~f =
  List.filter_map keys ~f:(fun k ->
      Option.map (f k) ~f:(fun v -> k, v)
    )

let assoc_map xs ~f =
  List.map xs ~f:(fun (k, v) -> k, f v)

let assoc_mapi xs ~f =
  List.map xs ~f:(fun (k, v) -> k, f k v)

let ( $ ) xs k = List.Assoc.find_exn xs k

module type Params = sig
  val dest : string
  val webroot : string
end

type path = string list
type 'a output = 'a Bistro_app.path

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

  module Model_X_transcriptome_deseq2_sample_pca = struct
    let path m = [ "model" ; m.model_id ; "transcriptome" ; "deseq2" ; "sample_pca.svg" ]

    let list =
      assoc_make' P.Model.list P.Transcriptome.deseq2
      |> assoc_map ~f:(fun deseq2 -> W.data deseq2#sample_pca)
  end

  module Model = struct
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

    let comparison_summary_table deseq2 =
      let%map Bistro_app.Path path = W.workflow deseq2#comparison_summary in
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
      let%map comparisons = W.assoc (assoc_map comparisons ~f:W.workflow) in
      List.map comparisons ~f:(fun ((name, l1, l2), table) ->
          sprintf "deseq2-comp-%s-%s-%s" name l1 l2,
          sprintf "%s: %s vs %s" name l1 l2,
          comparison_table table
        )
      |> tabs

    let mRNA_diff_expr_subsection deseq2 =
      let%map comparison_summary_table = comparison_summary_table deseq2
      and comparison_tabs = comparison_tabs deseq2#comparisons in
      subsection
        ~a:[a_id "mRNA-differential-expression-with-DESeq2"] "Differential expression with DESeq2"
        [ comparison_summary_table ; comparison_tabs ]

    let ( >>? ) x f =
      Option.value_map x ~default:(W.pure []) ~f

    let mRNA_section m =
      P.Transcriptome.deseq2 m >>? fun deseq2 ->
      let%map sample_pca_link = W.link (Model_X_transcriptome_deseq2_sample_pca.list $ m)
      and mRNA_diff_expr_subsection = mRNA_diff_expr_subsection deseq2 in
      section ~a:[a_id "mRNA"] "mRNA levels" [
        medskip ;
        [ p [ strong [ k"Sample overview " ] ; ] ] ;
        (* [ img ~a:[a_style "width:40%"] ~src:(W.href clustering) ~alt:"" () ] ; *)
        [ img ~a:[a_style "width:40%"] ~src:(W.href sample_pca_link) ~alt:"" () ] ;
        mRNA_diff_expr_subsection ;
      ]

    let title m = [
      h1 [b [k "Model " ; k m.model_id ]] ;
      hr () ;
    ]

    let sections m =
      let%map mRNA_section = mRNA_section m in
      List.concat [
        mRNA_section ;
      ]

    let path m = [ "model" ; m.model_id ^ ".html" ]

    let make m =
      let%map sections = sections m in
      let page_title = sprintf "Model :: %s" m.model_id in
      let contents = List.concat [ title m ; medskip ; medskip ; sections ] in
      W.html
        (path m)
        (html page_title contents)


    let list = assoc_make P.Model.list ~f:make
  end


  module Index = struct
    let path = [ "index.html" ]

    let browse_by_model_div =
      let%map links = assoc_map Model.list ~f:W.link |> W.assoc in
      let links = List.map links ~f:(fun (m, l) ->
          W.a l [ k m.model_id ]
        )
      in
      multicolumn_ul links

    let browse_by_div =
      let%map browse_by_model_div = browse_by_model_div in
      let tabs = tabs [
          (* "browse-by-sample", "Sample", [ browse_by_sample_div ] ; *)
          "browse-by-model",  "Model",  [ browse_by_model_div ] ;
        ]
      in
      div ((k "Browse by...") :: tabs)

    let page =
      let%map browse_by_div = browse_by_div in
      W.html path (
        html "Guizmin workflow" [
          h1 [b [k"Project " ; i [k P.project_name]]] ;
          hr () ;
          br () ;
          br () ;
          browse_by_div ;
        ]
      )

  end

end

let generate ~dest ~webroot (module Project : Unrolled_workflow.S) =
  let module Params = struct
    let dest = dest
    let webroot = webroot
  end
  in
  let module WWW = Make(Params)(Project) in
  W.generate ~dest WWW.Index.page
