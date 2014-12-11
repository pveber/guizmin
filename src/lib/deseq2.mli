open Workflow.Types

val r_library_package : package workflow

type wrapper_output = [`deseq2_wrapper_output] directory

val wrapper : string list -> (string list * Htseq.count_tsv workflow) list -> wrapper_output workflow

