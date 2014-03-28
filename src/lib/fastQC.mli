open Bistro_types

val package : package workflow

type report
type workflow = report directory Bistro_workflow.t

val run : 'a Fastq.workflow -> workflow
val html_report : workflow -> [`html] file Bistro_workflow.t
