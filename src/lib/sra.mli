open Bistro_workflow.Types

val package : package workflow

type format

type workflow = format file Bistro_workflow.t

val input : string -> workflow

val fetch_srr : string -> workflow

val fastq_dump : workflow -> [`sanger] Fastq.workflow
