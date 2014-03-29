open Bistro_types

type genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `sacCer2 ] with sexp
val string_of_genome : [< genome] -> string

(** {5 Twobit sequence format} *)
type twobit

(** {5 Download of genome sequences} *)
val chromosome_sequences : [< genome] -> [`ucsc_chromosome_sequences] directory workflow
val genome_sequence : [< genome] -> Fasta.workflow
(* val genome_2bit_sequence : [< genome] -> twobit file workflow *)

(* type bigWig *)
(* type wig *)



(* val wg_encode_crg_mappability_36  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_40  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_50  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_75  : [`mm9 | `hg18 | `hg19] -> bigWig file *)
(* val wg_encode_crg_mappability_100 : [`mm9 | `hg18 | `hg19] -> bigWig file *)

(* val fasta_of_bed : [< genome] -> 'a Bed.named_file -> Fasta.file *)
(* val fetch_sequences : [`ucsc_2bit] file_path -> Fungen.Location.t list -> string list *)

(* module Chrom_info : sig *)
(*   type tabular data = { *)
(*     chrom : string ; *)
(*     chrom_length : int *)
(*   } *)
(*   include module type of Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header) *)
(* end *)
(* val chrom_info : [< genome] -> Chrom_info.file *)
(* val bedClip : [< genome] -> 'a Bed.file -> 'a Bed.file *)

(* val wig_of_bigWig : bigWig file -> wig file *)
(* val bigWig_of_wig : ?clip:bool -> [< genome] -> wig file -> bigWig file *)

(* module Lift_over : sig *)
(*   open Fungen *)

(*   type chain_file = [`lift_over_chain] file *)
(*   val chain_file : org_from:[< genome] -> org_to:[< genome] -> chain_file *)

(*   (\** [conversion fp xs] returns a pair of location lists, mapped and *)
(*       unmapped locations. *\) *)
(*   val conversion : *)
(*     [`lift_over_chain] file_path -> *)
(*     Location.t Stream.t -> *)
(*     Location.t list * Location.t list *)

(*   (\** liftOver preserves {b more or less} the input BED: columns are *)
(*       conserved but fields may be changed (floats truncated to integers) *\) *)
(*   val bed_conversion : org_from:[< genome] -> org_to:[< genome] -> 'a Bed.file -> [`ucsc_lift_over of 'a] dir *)
(*   val mapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file *)
(*   val unmapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file *)
(* end *)
