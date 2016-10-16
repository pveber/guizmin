open Bistro.Std
open Bistro_bioinfo.Std

type fq2fa_input = [
  | `Se of [`sanger] fastq workflow
  | `Pe_merge of [`sanger] fastq workflow * [`sanger] fastq workflow
  | `Pe_paired of [`sanger] fastq workflow
]

val fq2fa : ?filter:bool -> fq2fa_input -> fasta workflow


type idba_ud_output = [`idba_ud_output] directory

val idba_ud : fasta workflow -> idba_ud_output workflow

val idba_ud_contigs : (idba_ud_output, fasta) selector
val idba_ud_scaffolds : (idba_ud_output, fasta) selector
