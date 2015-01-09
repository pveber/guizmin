open Core.Std
open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/macs2-install/2.1.0.20140616/macs2-install.sh"

let package =
  workflow [
    bash package_script [ dest ]
  ]

let macs2 cmd opts =
  program "macs2" ~path:[package] ~pythonpath:[package] (string cmd :: opts)

let pileup ?extsize ?both_direction bam =
  workflow [
    macs2 "pileup" [
      opt "-i" dep bam ;
      opt "-o" ident dest ;
      option (flag string "-B") both_direction ;
      option (opt "--extsize" int) extsize ;
    ]
  ]


type gsize = [`hs | `mm | `ce | `dm | `gsize of int]

let gsize_expr = function
  | `hs -> string "hs"
  | `mm -> string "mm"
  | `dm -> string "dm"
  | `ce -> string "ce"
  | `gsize n -> int n

let name = "macs2"

let callpeak ?pvalue ?qvalue ?gsize ?call_summits
             ?fix_bimodal ?extsize ?control treatment =
  workflow [
    macs2 "callpeak" [
      opt "--outdir" ident dest ;
      opt "--name" string name ;
      opt "--format" string "BAM" ;
      option (opt "--pvalue" float) pvalue ;
      option (opt "--qvalue" float) qvalue ;
      option (opt "--gsize" gsize_expr) gsize ;
      string "--bdg" ;
      option (flag string "--call-summits") call_summits ;
      option (opt "--extsize" int) extsize ;
      option (flag string "--fix-bimodal") fix_bimodal ;
      option (opt "--control" dep) control ;
      opt "--treatment" dep treatment ;
    ]
  ]

type peaks_xls = < columns : string * (int * (int * (int * (int * (int * (float * (float * (float * unit)))))))) ;
                  header : [`yes] ;
                  comment : [`sharp] ; .. > tsv

let peaks_xls o = Workflow.extract o [ name ^ "_peaks.xls" ]


type narrow_peaks = (string * (float * (float * (float * (int * unit))))) Bed.bed5_like

let narrow_peaks o =
  Workflow.extract o [ name ^ "_peaks.narrowPeak" ]
