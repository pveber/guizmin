open Bistro.Std
open Bistro_bioinfo.Std

type velvet_output = [`velvet_output] directory

val velvet :
  ?cov_cutoff:int ->
  ?min_contig_lgth:int ->
  hash_length:int ->
  ins_length:int ->
  exp_cov:float ->
  [`sanger] fastq workflow ->
  [`sanger] fastq workflow ->
  velvet_output workflow

val contigs : (velvet_output, fasta) selector
