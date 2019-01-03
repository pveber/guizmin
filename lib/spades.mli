open Bistro
open Bistro_bioinfo

val spades :
  ?single_cell:bool ->
  ?iontorrent:bool ->
  ?pe:sanger_fastq pworkflow list * sanger_fastq pworkflow list ->
  ?mem_spec:int ->
  unit ->
  [`spades] dworkflow

val contigs : [`spades] dworkflow -> fasta pworkflow
val scaffolds : [`spades] dworkflow -> fasta pworkflow
