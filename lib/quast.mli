open Bistro
open Bistro_bioinfo

val quast :
  ?reference:fasta pworkflow ->
  ?labels:string list ->
  fasta pworkflow list ->
  [`quast] dworkflow
