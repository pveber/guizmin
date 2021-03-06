open Core
open Bistro
open Bistro_bioinfo
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"ea-utils" ~tag:"20160226" ()

let adapters : fasta pworkflow =
  Bistro_unix.wget
    ~no_check_certificate:true
    "https://raw.githubusercontent.com/vsbuffalo/scythe/master/illumina_adapters.fa"

let fastq_mcf ?quality_threshold ?quality_mean fq1 fq2 =
  let fq1_file = "fq1.fastq.gz" in
  let fq2_file = "fq2.fastq.gz" in
  let inner =
    Workflow.shell ~descr:"ea-utils.fastq_mcf" [
      mkdir_p dest ;
      cmd ~env "fastq-mcf" [
        option (opt "-q" int) quality_threshold ;
        option (opt "--qual-mean" int) quality_mean ;
        dep adapters ;
        seq ~sep:"" [ string "<(gunzip -c " ; dep fq1 ; string ";)" ] ;
        seq ~sep:"" [ string "<(gunzip -c " ; dep fq2 ; string ";)" ] ;
        opt "-o" ident (dest // fq1_file) ;
        opt "-o" ident (dest // fq2_file) ;
      ]
    ]
  in
  Workflow.select inner [ fq1_file ],
  Workflow.select inner [ fq2_file ]

