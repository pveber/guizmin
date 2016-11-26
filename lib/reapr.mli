open Bistro.Std
open Bistro_bioinfo.Std

type output

val reapr :
  [`sanger] fastq workflow * [`sanger] fastq workflow ->
  fasta workflow ->
  output directory workflow

val assembly : (output, fasta) selector

