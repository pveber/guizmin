open Workflow.Types
open Defs
open Workflow.API

type index = [`bowtie_index] directory

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/bowtie-install/1.0.1/bowtie-install.sh"

let package = workflow [
    bash package_script [ dest ]
  ]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie_build ?packed ?color fa =
  workflow ~mem:(3 * 1024) ~timeout:24 [
    mkdir_p dest ;
    program "bowtie-build" ~path:[package] [
      option (flag string "-a -p") packed ;
      option (flag string "--color") color ;
      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq.Phred64 -> "--phred64-quals"

let bowtie ?l ?e ?m ?fastq_format ?n ?v ?p ?maxins index fastq_files =
  let args = match fastq_files with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list dep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list dep ~sep:",") fqs2
      ]
  in
  workflow ~mem:(3 * 1024) ~timeout:24 ?np:p [
    program "bowtie" ~path:[package] [
      string "-S" ;
      option (opt "-n" int) n ;
      option (opt "-l" int) l ;
      option (opt "-e" int) e ;
      option (opt "-m" int) m ;
      option (opt "-v" int) v ;
      option (opt "-q" (qual_option % string)) fastq_format ;
      option (opt "-p" int) p ;
      option (opt "--maxins" int) maxins ;
      seq [dep index ; string "/index"] ;
      args ;
      dest ;
    ]
  ]

(* let align_with_maq_policy ?l ?e ?m ?fastq_format ~n index fastq_files = *)
(*   f2 *)
(*     "guizmin.bioinfo.bowtie.align_with_maq_policy[r2]" *)
(*     Param.( *)
(*       [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ; *)
(*         opt qual_param "fastq_format" fastq_format] *)
(*     ) *)
(*     index (merge fastq_files) *)
(*     (fun env (Dir index) fastq_files path -> *)
(*       env.bash [ *)
(* 	<:sprint<bowtie -S -n $d:n$ \ *)
(*                         $? l <- l${-l $d:l$} \ *)
(*                         $? e <- e${-e $d:e$} \ *)
(*                         $? m <- m${-m $d:m$} \ *)
(* 	                $? q <- fastq_format${qual_option q} \ *)
(*                         -p $d:env.np$ \ *)
(* 	                $s:index$/index \ *)
(* 	                $!File f <- fastq_files ${$s:f$}{,} \ *)
(*                         $s:path$ >> *)
(*       ]) *)

(* let align ?m ?fastq_format ~v index fastq_files = *)
(*   f2 *)
(*     "guizmin.bioinfo.bowtie.align[r2]" *)
(*     Param.( *)
(*       [ int "v" v ; opt int "m" m ; *)
(*         opt qual_param "fastq_format" fastq_format ] *)
(*     ) *)
(*     index (merge fastq_files) *)
(*     (fun env (Dir index) fastq_files path -> *)
(*       let cmd = *)
(* 	<:sprint<bowtie -S -v $d:v$ $? m <- m${-m $d:m$} $? q <- fastq_format${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >> *)
(*       in *)
(*       env.bash [ cmd ]) *)



















