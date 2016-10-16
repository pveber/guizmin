open Bistro.Std
open Bistro_bioinfo.Std

type spades_output = [`spades_output] directory

val spades :
  ?single_cell:bool ->
  ?iontorrent:bool ->
  ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list ->
  unit ->
  spades_output workflow

val contigs : (spades_output, fasta) selector
val scaffolds : (spades_output, fasta) selector
