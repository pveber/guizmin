open Bistro
open Bistro_bioinfo

type _ tbool =
  | True  : [`True] tbool
  | False : [`False] tbool

type _ read_model =
  | Single_end : int -> [`single_end] read_model
  | Paired_end : paired_end -> [`paired_end] read_model

and paired_end = {
  len : int ;
  mflen : float ;
  sdev : float ;
  matepair : bool ;
}

type ('a, 'b, 'c, 'd) art_illumina_output = <
  directory ;
  contents : [`art_illumina] ;
  aln : 'a ;
  errfree_sam : 'b ;
  sam : 'c ;
  read_model : 'd ;
>

val art_illumina :
  ?qprof1:string ->
  ?qprof2:string ->
  ?amplicon:bool ->
  ?id:string ->
  ?insRate:float ->
  ?insRate2:float ->
  ?delRate:float ->
  ?delRate2:float ->
  ?maskN:int ->
  ?qShift:float ->
  ?qShift2:float ->
  ?rndSeed:float ->
  ?sepProf:bool ->
  ?seqSys:[< `GA1 | `GA2 | `HS10 | `HS20 | `HS25 | `MS ] ->
  ?cigarM:bool ->
  aln_output:'a tbool ->
  errfree_sam_output:'b tbool ->
  sam_output:'c tbool ->
  'rm read_model ->
  [< `Coverage_fold of float | `Read_count of int ] ->
  fasta pworkflow ->
  ('a, 'b, 'c, 'd) art_illumina_output pworkflow

val se_fastq :
  (_, _, _, [`single_end]) art_illumina_output pworkflow
  -> sanger_fastq pworkflow

val pe_fastq :
  [`One | `Two] ->
  (_, _, _, [`paired_end]) art_illumina_output pworkflow
  -> sanger_fastq pworkflow
