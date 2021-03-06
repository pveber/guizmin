open Core
open Bistro
open Bistro_bioinfo
open Bistro.Shell_dsl

let env = docker_image ~account:"pveber" ~name:"trinity" ~tag:"2.4.0" ()

let trinity ?(mem = 128) fa1 fa2 =
  let tmp_dest = tmp // "trinity" in
  Workflow.shell ~descr:"trinity" ~np:32 ~mem:(Workflow.int (mem * 1024)) [
    mkdir_p tmp ;
    cmd "Trinity" ~env ~stdout:(string "/dev/null")[
      string "--verbose" ;
      string "--seqType fa" ;
      opt "--left" dep fa1 ;
      opt "--right" dep fa2 ;
      opt "--CPU" ident np ;
      opt "--max_memory" ident (seq [ string "$((" ; Bistro.Shell_dsl.mem ; string " / 1024))G" ]) ;
      opt "--output" ident tmp_dest ;
    ] ;
    cmd "mv" ~env [
      tmp_dest // "Trinity.fasta" ;
      dest ;
    ]
  ]

let uniq_count_stats fa fq1 fq2 =
  let index = Bowtie2.bowtie2_build fa in
  let sam = Bowtie2.bowtie2 ~mode:`local ~no_unal:true index (`paired_end ([ fq1 ], [ fq2 ])) in
  let sorted_sam =
    Samtools.indexed_bam_of_sam sam
    |> Samtools.indexed_bam_to_bam
    |> Samtools.sam_of_bam
  in
  Workflow.shell ~descr:"trinity.uniq_count_stats.pl" [
    cmd "$TRINITY_HOME/util/SAM_nameSorted_to_uniq_count_stats.pl" ~env ~stdout:dest [
      dep sorted_sam ;
    ]
  ]
