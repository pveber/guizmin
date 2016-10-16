open Bistro.Std
open Bistro_bioinfo.Std

type output = [`reapr_output] directory

val reapr :
  [`sanger] fastq workflow * [`sanger] fastq workflow ->
  fasta workflow ->
  output workflow

val assembly : (output, fasta) selector

