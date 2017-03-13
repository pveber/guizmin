open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL


let gunzip_fastq_prefix n fq_gz =
  workflow ~descr:"gunzip_fastq_prefix" [
    pipe [
      cmd "zcat" [ dep fq_gz ] ;
      cmd "head" [ opt "-n" int (n * 4) ] ;
      cmd "gzip" ~stdout:dest [ string "--stdout" ] ;
    ]
  ]

let pipeline preview_mode species fq1_path fq2_path =
  let fq1_gz = input fq1_path in
  let fq2_gz = input fq2_path in
  let fq1_gz, fq2_gz =
    if preview_mode
    then
      let f = gunzip_fastq_prefix 1000000 in
      f fq1_gz, f fq2_gz
    else fq1_gz, fq2_gz
  in
  let initial_fastqc1 = MyFastQC.run fq1_gz in
  let initial_fastqc2 = MyFastQC.run fq2_gz in
  let trimmed_fq1_gz, trimmed_fq2_gz =
    Ea_utils.fastq_mcf ~quality_threshold:30 ~quality_mean:25 fq1_gz fq2_gz
  in
  let post_trimming_fastqc1 = MyFastQC.run trimmed_fq1_gz in
  let post_trimming_fastqc2 = MyFastQC.run trimmed_fq2_gz in
  let fa1 = Fastool.fastool trimmed_fq1_gz in
  let fa2 = Fastool.fastool trimmed_fq2_gz in
  let trinity_assembly = Trinity.trinity ~mem:(if preview_mode then 8 else 128) fa1 fa2 in
  let assembly_stats = Trinity.uniq_count_stats trinity_assembly (Unix_tools.gunzip trimmed_fq1_gz) (Unix_tools.gunzip trimmed_fq2_gz) in
  let kallisto_run = Kallisto.quant (Kallisto.index [ trinity_assembly ]) trimmed_fq1_gz trimmed_fq2_gz in
  let transdecoder_run = Transdecoder.transdecoder trinity_assembly in
  let canonical_transcripts =
    Transdecoder.(transdecoder_run / cds)
    |> Utils.rename_contigs species
    |> Utils.select_contig Kallisto.(kallisto_run / abundance)

  in
  Bistro_app.[
    [ "fastQC" ; "initial" ; "1" ] %> initial_fastqc1 ;
    [ "fastQC" ; "initial" ; "2" ] %> initial_fastqc2 ;
    [ "fastQC" ; "post_trimming" ; "1" ] %> post_trimming_fastqc1 ;
    [ "fastQC" ; "post_trimming" ; "2" ] %> post_trimming_fastqc2 ;
    [ "trinity_assembly.fa" ] %> trinity_assembly ;
    [ "assembly_stats" ] %> assembly_stats ;
    [ "transdecoder" ] %> transdecoder_run ;
    [ "kallisto" ] %> kallisto_run ;
    [ "canonical_transcripts.fa" ] %> canonical_transcripts ;
  ]

let main preview_mode outdir species np mem fq1_path fq2_path () =
  let targets = pipeline preview_mode species fq1_path fq2_path in
  let logger =
    Bistro_logger.tee
      (Bistro_console_logger.create ())
      (Bistro_html_logger.create "report.html")
  in
  Bistro_app.run ~logger ~np ~mem:(mem * 1024) (Bistro_app.of_repo ~outdir targets)

let spec =
  let open Command.Spec in
  empty
  +> flag "--preview-mode" no_arg ~doc:" Run on a small subset of the data"
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--species"  (required string) ~doc:"STRING Name of the species the samples are from"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"
  +> anon ("FQ1" %: file)
  +> anon ("FQ2" %: file)

let command =
  Command.basic
    ~summary:"De novo RNA-seq pipeline"
    spec
    main
