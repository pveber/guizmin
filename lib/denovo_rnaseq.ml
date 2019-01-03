open Core
open Bistro
open Bistro.Shell_dsl
open Bistro_utils

let gunzip_fastq_prefix n fq_gz =
  Workflow.shell ~descr:"gunzip_fastq_prefix" [
    pipe [
      cmd "zcat" [ dep fq_gz ] ;
      cmd "head" [ opt "-n" int (n * 4) ] ;
      cmd "gzip" ~stdout:dest [ string "--stdout" ] ;
    ]
  ]

let pipeline preview_mode species fq1_path fq2_path =
  let fq1_gz = Workflow.input fq1_path in
  let fq2_gz = Workflow.input fq2_path in
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
  let assembly_stats = Trinity.uniq_count_stats trinity_assembly (Bistro_unix.gunzip trimmed_fq1_gz) (Bistro_unix.gunzip trimmed_fq2_gz) in
  let kallisto_run = Kallisto.quant (Kallisto.index [ trinity_assembly ]) trimmed_fq1_gz trimmed_fq2_gz in
  let transdecoder_run = Transdecoder.transdecoder trinity_assembly in
  let canonical_transcripts =
    Transdecoder.cds transdecoder_run
    |> Utils.rename_contigs species
    |> Utils.select_contig (Kallisto.abundance kallisto_run)

  in
  Repo.[
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

let main ~preview_mode ~outdir ~species ~np ~mem ~fq1_path ~fq2_path () =
  let targets = pipeline preview_mode species fq1_path fq2_path in
  let loggers = [
    Console_logger.create () ;
    Html_logger.create "report.html" ;
  ]
  in
  Repo.build_main ~collect:false ~loggers ~np ~mem:(`GB mem) ~outdir targets

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"De novo RNA-seq pipeline"
    [%map_open
      let preview_mode = flag "--preview-mode" no_arg ~doc:" Run on a small subset of the data"
      and outdir = flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
      and species = flag "--species"  (required string) ~doc:"STRING Name of the species the samples are from"
      and np = flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
      and mem = flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"
      and fq1_path = anon ("FQ1" %: file)
      and fq2_path = anon ("FQ2" %: file)
      in
      main ~preview_mode ~outdir ~species ~np ~mem ~fq1_path ~fq2_path
    ]
