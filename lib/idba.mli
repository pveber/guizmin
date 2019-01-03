open Bistro
open Bistro_bioinfo

type fq2fa_input = [
  | `Se of sanger_fastq pworkflow
  | `Pe_merge of sanger_fastq pworkflow * sanger_fastq pworkflow
  | `Pe_paired of sanger_fastq pworkflow
]

val fq2fa : ?filter:bool -> fq2fa_input -> fasta pworkflow

val idba_ud : ?mem_spec:int -> fasta pworkflow -> [`idba] dworkflow

val idba_ud_contigs : [`idba] dworkflow -> fasta pworkflow
val idba_ud_scaffolds : [`idba] dworkflow -> fasta pworkflow
