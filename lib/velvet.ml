open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

type velvet_output = [`velvet_output] directory

let velvet ?(mem_spec = 10) ?cov_cutoff ?min_contig_lgth ~hash_length ~ins_length ~exp_cov fq1 fq2 =
  workflow ~mem:(mem_spec * 1024) ~descr:"velvet" [
    mkdir_p dest ;
    cmd "velveth" [
      dest ;
      int hash_length ;
      string "-separate" ;

      string "-shortPaired" ;
      string "-fastq" ;
      string "-short" ;
      dep fq1 ;
      dep fq2 ;
    ] ;
    cmd "velvetg" [
      dest ;
      opt "-ins_length" int ins_length ;
      opt "-exp_cov" float exp_cov ;
      option (opt "-cov_cutoff" int) cov_cutoff ;
      option (opt "-min_contig_lgth" int) min_contig_lgth ;
    ]
  ]

let contigs = selector ["contigs.fa"]
