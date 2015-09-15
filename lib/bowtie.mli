open Bistro_std

type index = [`bowtie_index] directory

val package : package workflow

val bowtie_build :
  ?packed:bool ->
  ?color:bool  ->
  Fasta.workflow -> index workflow

val bowtie :
  ?l:int -> ?e:int -> ?m:int ->
  ?fastq_format:'a Fastq.format ->
  ?n:int -> ?v:int -> ?p:int ->
  ?maxins:int ->
  index workflow ->
  [ `single_end of 'a Fastq.workflow list
  | `paired_end of 'a Fastq.workflow list * 'a Fastq.workflow list ] ->
  Sam.workflow
