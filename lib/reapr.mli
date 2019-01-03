open Bistro
open Bistro_bioinfo

val reapr :
  sanger_fastq pworkflow * sanger_fastq pworkflow ->
  fasta pworkflow ->
  [`reapr] dworkflow

val assembly : [`reapr] dworkflow -> fasta pworkflow

