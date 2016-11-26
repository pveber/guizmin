open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"transdecoder" ~tag:"3.0.1" ()

type transdecoder_output

let transdecoder fa : transdecoder_output directory workflow =
  workflow ~descr:"transdecoder.longOrfs" [
    mkdir_p tmp ;
    cmd "ln" [ string "-s " ; dep fa ; tmp // "transcripts.fa" ] ;
    and_list [
      cmd "cd" [ tmp ] ;
      cmd "TransDecoder.LongOrfs" ~env [ opt "-t" string "transcripts.fa" ] ;
      cmd "TransDecoder.Predict"  ~env [ opt "-t" string "transcripts.fa" ] ;
    ] ;
    mkdir_p dest ;
    mv (tmp // "transcripts.fa.transdecoder.*") dest ;
  ]

let cds : (transdecoder_output, fasta) selector =
  selector ["transcripts.fa.transdecoder.cds"]
