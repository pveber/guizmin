open Bistro_workflow.Types

val package : package workflow

type report
type workflow = report directory Bistro_workflow.t

val run : 'a Fastq.workflow -> workflow
val html_report : workflow -> html Bistro_workflow.t
val per_base_quality : workflow -> [`png] Bistro_workflow.t
val per_base_sequence_content : workflow -> [`png] Bistro_workflow.t
