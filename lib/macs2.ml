open Core.Std
open Std
open Bistro.EDSL_sh

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/macs2-install/2.1.0.20140616/macs2-install.sh"

let package = Workflow.make ~descr:"macs2.package" [%sh{|
PREFIX={{ dest }}

URL=https://pypi.python.org/packages/source/M/MACS2/MACS2-2.1.0.20140616.tar.gz
ARCHIVE=`basename ${URL}`
PACKAGE=MACS2

mkdir -p $PREFIX/src
cd $PREFIX/src
wget ${URL}
tar xvfz ${ARCHIVE}
rm $ARCHIVE
cd ${ARCHIVE%\.tar.gz}
python setup.py install --prefix ${PREFIX}
|}]

let macs2 subcmd opts =
  cmd "macs2" ~path:[package] ~pythonpath:[package] (string subcmd :: opts)

let pileup ?extsize ?both_direction bam =
  workflow ~descr:"macs2.pileup" [
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
  workflow ~descr:"macs2.callpeak" [
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
