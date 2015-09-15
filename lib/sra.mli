open Bistro_std

val toolkit_package : package workflow

type workflow = ([`sra], [`binary]) file Workflow.t

val input : string -> workflow

val fetch_srr : string -> workflow

val fastq_dump : workflow -> [`sanger] Fastq.workflow

val fastq_dump_pe : workflow -> [`sanger] Fastq.workflow * [`sanger] Fastq.workflow
