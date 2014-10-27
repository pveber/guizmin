open Workflow.Types

val package : package workflow

val pileup :
  ?extsize:int ->
  ?both_direction:bool ->
  Bam.workflow -> Bedgraph.workflow
