open Workflow.Types

val tophat1 :
  ?num_threads:int ->
  ?color:bool ->
  Bowtie.index workflow ->
  [ `single_end of 'a Fastq.workflow list
  | `paired_end of 'a Fastq.workflow list * 'a Fastq.workflow list ] ->
  [`tophat_output] directory workflow

val tophat2 :
  ?num_threads:int ->
  Bowtie2.index workflow ->
  [ `single_end of 'a Fastq.workflow list
  | `paired_end of 'a Fastq.workflow list * 'a Fastq.workflow list ] ->
  [`tophat_output] directory workflow

val accepted_hits :
  [`tophat_output] directory workflow ->
  Bam.workflow
