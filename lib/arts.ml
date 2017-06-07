open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"art" ~tag:"20160605" ()

let depth_option = function
  | `Read_count i -> opt "--rcount" int i
  | `Coverage_fold f -> opt "--fcov" float f

let seqSys_option = function
  | `GA1 -> "GA1"
  | `GA2 -> "GA2"
  | `HS10 -> "HS10"
  | `HS20 -> "HS20"
  | `HS25 -> "HS25"
  | `MS -> "MS"

type _ tbool =
  | True  : [`True] tbool
  | False : [`False] tbool

let ite
  : type b. b tbool -> 'a -> 'a -> 'a
  = fun b x y ->
    match b with
    | True -> x
    | False -> y

type 'a art_illumina_output =
  [ `art_illumina_output of 'a ]
  constraint 'a = < aln : _ ;
                    errfree_sam : _ ;
                    sam : _ ;
                    read_model : _ >

type _ read_model =
  | Single_end : int -> [`single_end] read_model
  | Paired_end : paired_end -> [`paired_end] read_model

and paired_end = {
  len : int ;
  mflen : float ;
  sdev : float ;
  matepair : bool ;
}

let args_of_read_model
  : type u. u read_model -> Bistro.Template.t list
  = function
    | Single_end len ->
      [ opt "--len" int len ]
    | Paired_end pe ->
      [ string "--paired" ;
        opt "--len" int pe.len ;
        opt "--mflen" float pe.mflen ;
        opt "--sdev" float pe.sdev ;
        flag string "--matepair" pe.matepair ]


let art_illumina
    ?qprof1 ?qprof2 ?amplicon ?id
    ?insRate ?insRate2 ?delRate ?delRate2
    ?maskN ?qShift ?qShift2 ?rndSeed
    ?sepProf ?seqSys ?cigarM
    ~(aln_output : 'a tbool)
    ~(errfree_sam_output : 'b tbool)
    ~(sam_output : 'c tbool)
    (read_model : 'rm read_model) depth (fa : fasta workflow)

  : < aln : 'a ;
      errfree_sam : 'b ;
      sam : 'c ;
      read_model : 'rm > art_illumina_output directory workflow
  =
  workflow ~descr:"art_illumina" [
    mkdir_p dest ;
    cmd "art_illumina" ~env [
      option (opt "--qprof1" string) qprof1 ;
      option (opt "--qprof2" string) qprof2 ;
      option (flag string "--amplicon") amplicon ;
      option (opt "--id" string) id ;
      option (opt "--insRate" float) insRate ;
      option (opt "--insRate2" float) insRate2 ;
      option (opt "--delRate" float) delRate ;
      option (opt "--delRate2" float) delRate2 ;
      option (opt "--maskN" int) maskN ;
      option (opt "--qShift" float) qShift ;
      option (opt "--qShift2" float) qShift2 ;
      option (opt "--rndSeed" float) rndSeed ;
      option (flag string "--sepProf") sepProf ;
      option (opt "--seqSys" (seqSys_option % string)) seqSys ;
      option (flag string "--cigarM") cigarM ;
      string (ite aln_output "" "--noALN") ;
      string (ite errfree_sam_output "--errfree" "") ;
      string (ite sam_output "--samout" "") ;
      depth_option depth ;
      seq ~sep:" " (args_of_read_model read_model) ;
      opt "--in" dep fa ;
      opt "--out" (fun x -> seq [x ; string "/sample"]) dest ;
    ]
  ]


let se_fastq () = selector [ "sample.fq" ]

let pe_fastq x =
  selector [
    match x with
    | `One -> "sample1.fq"
    | `Two -> "sample2.fq"
  ]
