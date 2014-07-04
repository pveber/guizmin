open Bistro_workflow.Types

type genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `sacCer2 ] with sexp

let string_of_genome = function
| `dm3 -> "dm3"
| `hg18 -> "hg18"
| `hg19 -> "hg19"
| `mm8 -> "mm8"
| `mm9 -> "mm9"
| `sacCer2 -> "sacCer2"


let package = Bistro_workflow.make <:script<
URL=git://genome-source.cse.ucsc.edu/kent.git
PREFIX=#DEST

git clone ${URL} || (echo "Failed to git kent repository" && exit 1)
BINDIR=${PREFIX}/bin 
MACHTYPE=`echo ${MACHTYPE} | cut -d '-' -f 1`
MYSQLLIBS=`mysql_config --libs` || (echo "improper mysql install" && exit 1)
MYSQLINC=`mysql_config --include | sed -e 's/-I//g'` || (echo "improper mysql install" && exit 1)
sed -i -e 's/-Werror//g' kent/src/inc/common.mk
sed -i -e 's/\\$A: \\$O \\${MYLIBS}/\\$A: \\$O/g' kent/src/hg/pslCDnaFilter/makefile
make -C kent/src userApps \
	BINDIR="${BINDIR}" \
	SCRIPTS="${BINDIR}" \
	MACHTYPE="${MACHTYPE}" \
	MYSQLLIBS="${MYSQLLIBS} -lz" \
	MYSQLINC="${MYSQLINC}"
make -C kent/src/hg/genePredToGtf \
	BINDIR="${BINDIR}" \
	SCRIPTS="${BINDIR}" \
	MACHTYPE="${MACHTYPE}" \
	MYSQLLIBS="${MYSQLLIBS} -lz" \
	MYSQLINC="${MYSQLINC}"
make -C kent/src/hg/gpToGtf \
	BINDIR="${BINDIR}" \
	SCRIPTS="${BINDIR}" \
	MACHTYPE="${MACHTYPE}" \
	MYSQLLIBS="${MYSQLLIBS} -lz" \
	MYSQLINC="${MYSQLINC}"
>>


type twobit = ([`twobit], [`binary]) file

let chromosome_sequences org =
  let org = string_of_genome org in Bistro_workflow.make <:script<
    mkdir -p #DEST
    cd #DEST
    wget ftp://hgdownload.cse.ucsc.edu/goldenPath/#s:org#/chromosomes/*
    gunzip *.gz
  >>

let genome_sequence org =
  let chr_seqs = chromosome_sequences org in
  Bistro_workflow.make <:script<
    bash -c "shopt -s nullglob ; cat #w:chr_seqs#/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > #DEST"
  >>

(* UGLY hack due to twoBitToFa: this tool requires that the 2bit
   sequence should be put in a file with extension 2bit. So I'm forced
   to create first a directory and then to select the unique file in it...*)
let genome_2bit_sequence_dir org =
  let org = string_of_genome org in
  Bistro_workflow.make <:script<
mkdir #DEST
cd #DEST
wget ftp://hgdownload.cse.ucsc.edu/goldenPath/#s:org#/bigZips/#s:org#.2bit
>>

let genome_2bit_sequence org = Bistro_workflow.select (genome_2bit_sequence_dir org) ((string_of_genome org) ^ ".2bit")

(* type bigWig *)
(* type wig *)





(* let wg_encode_crg_mappability n org = *)
(*   let url = sp "ftp://hgdownload.cse.ucsc.edu/gbdb/%s/bbi/wgEncodeCrgMapabilityAlign%dmer.bigWig" (string_of_genome org) n in *)
(*   Guizmin_unix.wget url *)

(* let wg_encode_crg_mappability_36 org = wg_encode_crg_mappability 36 org *)
(* let wg_encode_crg_mappability_40 org = wg_encode_crg_mappability 40 org *)
(* let wg_encode_crg_mappability_50 org = wg_encode_crg_mappability 50 org *)
(* let wg_encode_crg_mappability_75 org = wg_encode_crg_mappability 75 org *)
(* let wg_encode_crg_mappability_100 org = wg_encode_crg_mappability 100 org *)

let twoBitToFa bed twobits = Bistro_workflow.make <:script<
export PATH=#w:package#:$PATH
twoBitToFa -bed=#w:bed# #w:twobits# #DEST
>>

let fasta_of_bed org bed =
  twoBitToFa bed (genome_2bit_sequence org)

(*   f2 *)
(*     "guizmin.bioinfo.ucsc.fasta_of_bed[1]" [] *)
(*     seq2b bed *)
(*     (fun env (File seq2b) (File bed) path -> *)
(*       twoBitToFa ~positions:(`bed bed) ~seq2b ~fa:path) *)

(* let fetch_sequences (File seq2b) locations = *)
(*   let open Core.Std in *)
(*   Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".seqList" (fun seqList -> *)
(*     Core_extended.Sys_utils.with_tmp ~pre:"gzm" ~suf:".fa" (fun fa -> *)
(*       (\* Write locations to a file *\) *)
(*       List.map locations Fungen.Location.to_string *)
(*       |> Out_channel.write_lines seqList ; *)

(*       (\* run twoBitToFa *\) *)
(*       twoBitToFa ~positions:(`seqList seqList) ~seq2b ~fa ; *)

(*       (\* Parse the Fasta file *\) *)
(*       In_channel.with_file fa ~f:(fun ic -> *)
(*         Biocaml.Fasta.(in_channel_to_char_seq_item_stream_exn ic) *)
(*         /@ (fun x -> x.Biocaml.Fasta.sequence) *)
(*         |> Stream.to_list *)
(*       ) *)
(*     ) *)
(*   ) *)

(* module Chrom_info = struct *)
(*   type tabular data = { *)
(*     chrom : string ; *)
(*     chrom_length : int *)
(*   } *)
(*   include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header) *)
(* end *)

(* let chrom_info_cmd1 org = Printf.sprintf "\ *)
(* mysql --user=genome --host=genome-mysql.cse.ucsc.edu -A -N \ *)
(* -e 'select chrom,size from chromInfo;' %s" (string_of_genome org) *)
(* let chrom_info_cmd2 path = Printf.sprintf "\ *)
(* gawk -F'\t' '{printf \"%%s\\t%%s\\n\", $1,$2}' >> %s" path *)

(* let chrom_info org = *)
(*   f0 *)
(*     "guizmin.bioinfo.ucsc.chrom_info[r1]" *)
(*     [ Param.string "org" (string_of_genome org) ] *)
(*     (fun env path -> *)
(*        let cmd = *)
(*          pipefail *)
(*            (chrom_info_cmd1 org) *)
(*            (chrom_info_cmd2 path) *)
(*        in *)
(*        ignore (Sys.command cmd)) *)

(* let bedClip org bed = *)
(*   let chrom_info = chrom_info org in *)
(*   f2 *)
(*     "guizmin.bioinfo.ucsc.bedClip[r1]" [] *)
(*     chrom_info bed *)
(*     (fun env (File chrom_info) (File bed) path -> *)
(*        env.sh "bedClip -verbose=2 %s %s %s" bed chrom_info path) *)

(* let wig_of_bigWig bigWig = *)
(*   f1 *)
(*     "guizmin.bioinfo.ucsc.wig_of_bigWig[r1]" [] *)
(*     bigWig *)
(*     ( *)
(*       fun env (File bigWig) path -> *)
(* 	env.bash [ *)
(*           sp "bigWigToWig %s %s" bigWig path *)
(* 	] *)
(*     ) *)

(* let bigWig_of_wig ?(clip = false) org wig = *)
(*   let chrom_info = chrom_info org in *)
(*   f2 *)
(*     "guizmin.bioinfo.ucsc.bigWig_of_wig[r1]" [] *)
(*     chrom_info wig *)
(*     (fun env (File chrom_info) (File wig) path -> *)
(*       let clip = if clip then "-clip" else "" in *)
(*       env.sh "wigToBigWig %s %s %s %s" clip wig chrom_info path) *)

(* module Lift_over = struct *)
(*   open Printf *)
(*   open Core.Std *)
(*   open CFStream *)
(*   open Stream.Infix *)
(*   open Fungen *)

(*   type chain_file = [`lift_over_chain] file *)

(*   let chain_file ~org_from ~org_to = *)
(*     let org_from = string_of_genome org_from *)
(*     and org_to = string_of_genome org_to in *)
(*     let url = *)
(*       sprintf *)
(*         "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/liftOver/%sTo%s.over.chain.gz" *)
(*         org_from org_from (String.capitalize org_to) *)
(*     in *)
(*     Guizmin_unix.(gunzip (wget url)) *)

(*   let create_input_file xs = *)
(*     let fn = Filename.temp_file "gzm" ".locations" in *)
(*     xs /@ Location.to_string *)
(*     |! lines_to_file fn ; *)
(*     fn *)

(*   let liftOver_cmd ~output ~chain_file ~old_locs ~new_locs ~unmapped_locs = *)
(*     let string_of_output = function *)
(*     | `bed -> "" *)
(*     | `position -> "-positions" *)
(*     in *)
(*     sprintf *)
(*       "liftOver %s %s %s %s %s" *)
(*       (string_of_output output) *)
(*       old_locs chain_file new_locs unmapped_locs *)

(*   let conversion (File chain_file) xs = *)
(*     let old_locs_fn = create_input_file xs in *)
(*     let new_locs_fn = Filename.temp_file "gzm" ".locations" *)
(*     and unmapped_locs_fn = (\* unmapped locations *\) *)
(*       Filename.temp_file "gzm" ".locations" in *)
(*     let cmd = liftOver_cmd ~output:`position ~chain_file ~old_locs:old_locs_fn ~new_locs:new_locs_fn ~unmapped_locs:unmapped_locs_fn in *)
(*     sh "%s 2> /dev/null" cmd ; *)
(*     let new_locs = *)
(*       List.map (lines_of_file new_locs_fn) ~f:Location.of_string *)
(*     and unmp_locs = *)
(*       List.map (lines_of_file unmapped_locs_fn) ~f:Location.of_string *)
(*     in *)
(*     sh "rm -f %s %s %s liftOver_*" old_locs_fn new_locs_fn unmapped_locs_fn ; *)
(*     new_locs, unmp_locs *)

(*   let bed_conversion ~org_from ~org_to bed = *)
(*     let chain_file = chain_file ~org_from ~org_to in *)
(*     d2 *)
(*       "guizmin.bioinfo.ucsc.bed_conversion[r2]" [] *)
(*       chain_file bed *)
(*       (fun env (File chain_file) (File bed) path -> *)
(*         env.sh "mkdir -p %s" path ; *)
(*         env.sh "%s" ( *)
(*           liftOver_cmd *)
(*             ~output:`bed *)
(*             ~chain_file *)
(*             ~old_locs:bed *)
(*             ~new_locs:(path ^ "/mapped.bed") *)
(*             ~unmapped_locs:(path ^ "/unmapped.bed") *)
(*         )) *)

(*   let mapped x = select x "mapped.bed" *)
(*   let unmapped x = select x "unmapped.bed" *)
(* end *)

