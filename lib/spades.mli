open Bistro.Std
open Bistro_bioinfo.Std

type spades_output

val spades :
  ?single_cell:bool ->
  ?iontorrent:bool ->
  ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list ->
  ?mem_spec:int ->
  unit ->
  spades_output directory workflow

val contigs : (spades_output, fasta) selector
val scaffolds : (spades_output, fasta) selector
