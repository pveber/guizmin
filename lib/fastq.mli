open Bistro_std

type _ format =
  | Sanger  : [`sanger] format
  | Solexa  : [`solexa] format
  | Phred64 : [`phred64] format

type 'a workflow = ([`fastq of 'a format], [`text]) file Workflow.t

val sanger_of_solexa : [`solexa] workflow -> [`sanger] workflow
val sanger_of_solexa : [`phred64] workflow -> [`sanger] workflow
val to_sanger : 'a format -> 'a workflow -> [`sanger] workflow

val concat : 'a workflow list -> 'a workflow
