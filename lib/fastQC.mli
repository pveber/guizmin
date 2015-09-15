open Std

val package : package workflow

type workflow = [`fastQC_report] directory Workflow.t

val run : 'a Fastq.workflow -> workflow
val html_report : workflow -> html Workflow.t
val per_base_quality : workflow -> png Workflow.t
val per_base_sequence_content : workflow -> png Workflow.t
