open Workflow.Types
open Ucsc_gb.Types

val package : package workflow

val pileup :
  ?extsize:int ->
  ?both_direction:bool ->
  Bam.workflow -> bedGraph workflow
