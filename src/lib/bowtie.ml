open Workflow.Types
open Workflow.API

type index = [`bowtie_index] directory

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/bowtie-install/1.0.1/bowtie-install.sh"

let package = workflow [
    bash package_script [ target () ]
  ]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie_build ?packed ?color fa =
  workflow ~mem:(3 * 1024) [
    mkdir_p (target ()) ;
    program "bowtie-build" ~path:[package] [
      option (flag string "-a -p") packed ;
      option (flag string "--color") color ;
      opt "-f" dep fa ;
      seq [ target () ; string "/index" ]
    ]
  ]

(* let qual_option (type s) x = match (x : s Fastq.format) with *)
(*   | Fastq.Solexa  -> "--solexa-quals" *)
(*   | Fastq.Sanger -> "--phred33-quals" *)
(*   | Fastq.Phred64 -> "--phred64-quals" *)

(* let bowtie ?l ?e ?m ?fastq_format ?n ?v ?p index fastq_files = *)
(*   Bistro_workflow.make ~mem:(3 * 1024) ~timeout:`day ?np:p <:script< *)

(*   export PATH=#w:package#/bin:$PATH *)
(*   bowtie -S \ *)
(*     #? n <- n #[-n #i:n#] \ *)
(*     #? l <- l #[-l #i:l#] \ *)
(*     #? e <- e #[-e #i:e#] \ *)
(*     #? m <- m #[-m #i:m#] \ *)
(*     #? v <- v #[-v #i:v#] \ *)
(*     #? q <- fastq_format #[qual_option q] \ *)
(*     #? p <- p #[-p #i:p#] \ *)
(*     #w:index#/index \ *)
(*     #! f <- fastq_files #[#w:f#][,] \ *)
(*     #DEST *)
(* >> *)

(* (\* let align_with_maq_policy ?l ?e ?m ?fastq_format ~n index fastq_files = *\) *)
(* (\*   f2 *\) *)
(* (\*     "guizmin.bioinfo.bowtie.align_with_maq_policy[r2]" *\) *)
(* (\*     Param.( *\) *)
(* (\*       [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ; *\) *)
(* (\*         opt qual_param "fastq_format" fastq_format] *\) *)
(* (\*     ) *\) *)
(* (\*     index (merge fastq_files) *\) *)
(* (\*     (fun env (Dir index) fastq_files path -> *\) *)
(* (\*       env.bash [ *\) *)
(* (\* 	<:sprint<bowtie -S -n $d:n$ \ *\) *)
(* (\*                         $? l <- l${-l $d:l$} \ *\) *)
(* (\*                         $? e <- e${-e $d:e$} \ *\) *)
(* (\*                         $? m <- m${-m $d:m$} \ *\) *)
(* (\* 	                $? q <- fastq_format${qual_option q} \ *\) *)
(* (\*                         -p $d:env.np$ \ *\) *)
(* (\* 	                $s:index$/index \ *\) *)
(* (\* 	                $!File f <- fastq_files ${$s:f$}{,} \ *\) *)
(* (\*                         $s:path$ >> *\) *)
(* (\*       ]) *\) *)

(* (\* let align ?m ?fastq_format ~v index fastq_files = *\) *)
(* (\*   f2 *\) *)
(* (\*     "guizmin.bioinfo.bowtie.align[r2]" *\) *)
(* (\*     Param.( *\) *)
(* (\*       [ int "v" v ; opt int "m" m ; *\) *)
(* (\*         opt qual_param "fastq_format" fastq_format ] *\) *)
(* (\*     ) *\) *)
(* (\*     index (merge fastq_files) *\) *)
(* (\*     (fun env (Dir index) fastq_files path -> *\) *)
(* (\*       let cmd = *\) *)
(* (\* 	<:sprint<bowtie -S -v $d:v$ $? m <- m${-m $d:m$} $? q <- fastq_format${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >> *\) *)
(* (\*       in *\) *)
(* (\*       env.bash [ cmd ]) *\) *)



















