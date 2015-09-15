open Core.Std
open Std
open Bistro_std
open Bistro.EDSL_sh

type index = [`bowtie2_index] directory

let package = Workflow.make ~descr:"bowtie2.package" [%sh{|
PREFIX={{ dest }}

mkdir -p $PREFIX
cd $PREFIX
wget -O bowtie2.zip "http://downloads.sourceforge.net/project/bowtie-bio/bowtie2/2.2.4/bowtie2-2.2.4-linux-x86_64.zip?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fbowtie-bio%2Ffiles%2Fbowtie2%2F2.2.4%2F&ts=1417768070&use_mirror=freefr" || die "failed to download archive"
unzip bowtie2.zip
rm bowtie2.zip
mv bowtie2-2.2.4 bin
|}]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie2_build ?large_index ?noauto ?packed ?bmax ?bmaxdivn ?dcv ?nodc ?noref ?justref ?offrate ?ftabchars ?seed ?cutoff fa =
  workflow ~descr:"bowtie2_build" ~mem:(3 * 1024) ~timeout:24 [
    mkdir_p dest ;
    cmd "bowtie2-build" ~path:[package] [
      option (flag string "--large-index") large_index ;
      option (flag string "--no-auto") noauto ;
      option (flag string "--packed") packed ;
      option (flag string "--nodc") nodc ;
      option (flag string "--noref") noref ;
      option (flag string "--justref") justref ;
      option (opt "--bmax" int) bmax ;
      option (opt "--bmaxdivn" int) bmaxdivn ;
      option (opt "--dcv" int) dcv ;
      option (opt "--offrate" int) offrate ;
      option (opt "--ftabchars" int) ftabchars ;
      option (opt "--seed" int) seed ;
      option (opt "--cutoff" int) cutoff ;

      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq.Phred64 -> "--phred64-quals"

let flag_of_preset mode preset =
  let flag = match preset with
    | `very_fast -> "--very-fast"
    | `fast -> "--fast"
    | `sensitive -> "--sensitive"
    | `very_sensitive -> "--very-sensitive"
  in
  if mode = `local then flag ^ "-local" else flag

let flag_of_mode = function
  | `end_to_end -> "--end-to-end"
  | `local -> "--local"

let flag_of_orientation = function
  | `fr -> "--fr"
  | `rf -> "--rf"
  | `ff -> "--ff"

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie2
    ?skip ?qupto ?trim5 ?trim3 ?preset
    ?_N ?_L ?ignore_quals ?(mode = `end_to_end)
    ?a ?k ?_D ?_R ?minins ?maxins ?orientation
    ?no_mixed ?no_discordant ?dovetail ?no_contain ?no_overlap
    ?threads ?seed
    ?fastq_format index fqs =

  let args = match fqs with
    | `single_end fqs ->
      opt "-U" (list dep ~sep:",") fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list dep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list dep ~sep:",") fqs2
      ]
  in
  workflow ~descr:"bowtie2" ~mem:(3 * 1024) ~timeout:24 ?np:threads [
    cmd "bowtie2" ~path:[package] [
      option (opt "--skip" int) skip ;
      option (opt "--qupto" int) qupto ;
      option (opt "--trim5" int) trim5 ;
      option (opt "--trim3" int) trim3 ;
      option ((flag_of_preset mode) % string) preset ;
      option (opt "-N" int) _N ;
      option (opt "-L" int) _L ;
      option (flag string "--ignore-quals") ignore_quals ;
      (flag_of_mode % string) mode ;
      option (flag string "-a") a ;
      option (opt "-k" int) k ;
      option (opt "-D" int) _D ;
      option (opt "-R" int) _R ;
      option (opt "--minins" int) minins ;
      option (opt "--maxins" int) maxins ;
      option (flag_of_orientation % string) orientation ;
      option (flag string "--no-mixed") no_mixed  ;
      option (flag string "--no-discordant") no_overlap  ;
      option (flag string "--dovetail") dovetail ;
      option (flag string "--no-contain") no_contain ;
      option (flag string "--no-overlap") no_overlap ;
      option (opt "--threads" int) threads ;
      option (opt "--seed" int) seed ;
      option (opt "-q" (qual_option % string)) fastq_format ;
      opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
      args ;
      opt "-S" ident dest ;
    ]
  ]
