open Bistro
open Bistro_bioinfo

val velvet :
  ?mem_spec:int ->
  ?cov_cutoff:int ->
  ?min_contig_lgth:int ->
  hash_length:int ->
  ins_length:int ->
  exp_cov:float ->
  sanger_fastq pworkflow ->
  sanger_fastq pworkflow ->
  [`velvet] dworkflow

val contigs : [`velvet] dworkflow -> fasta pworkflow
