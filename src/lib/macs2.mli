open Workflow.Types
open Ucsc_gb.Types

val package : package workflow

val pileup :
  ?extsize:int ->
  ?both_direction:bool ->
  Bam.workflow -> bedGraph workflow

type gsize = [`hs | `mm | `ce | `dm | `gsize of int]

val callpeak :
  ?pvalue:float ->
  ?qvalue:float ->
  ?gsize:gsize ->
  ?call_summits:bool ->
  ?fix_bimodal:bool ->
  ?extsize:int ->
  ?control:Bam.workflow ->
  Bam.workflow ->
  [`macs2_callpeak_output] directory workflow

type peaks_xls = < columns : string * (int * (int * (int * (int * (int * (float * (float * (float * unit)))))))) ;
                  header : [`yes] ;
                  comment : [`sharp] ; .. > tsv

val peaks_xls :
  [`macs2_callpeak_output] directory workflow -> peaks_xls workflow

val narrow_peaks :
  [`macs2_callpeak_output] directory workflow -> Bed.bed3 workflow

