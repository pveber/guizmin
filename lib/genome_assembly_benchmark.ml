open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std

let ( / ) = Bistro.EDSL.( / )

type dataset = {
  name : string ;
  reference : fasta workflow ;
  genome_size : int ;
  reads : [`sanger] fastq workflow * [`sanger] fastq workflow ;
}

let fetch_fq_gz url : [`sanger] fastq workflow =
  Unix_tools.wget url
  |> Unix_tools.gunzip

let pipeline { name ; genome_size ; reference ; reads = ((reads_1, reads_2) as reads) } =
  let spades_assembly =
    let pe = [ reads_1 ], [ reads_2 ] in
    Spades.spades ~pe () / Spades.contigs
  in
  let idba_ud_assembly =
    Idba.(idba_ud (fq2fa (`Pe_merge reads)))
    / Idba.idba_ud_contigs
  in
  let velvet_assembly =
    Velvet.velvet
    ~cov_cutoff:4
    ~min_contig_lgth:100
    ~hash_length:21
    ~ins_length:400
    ~exp_cov:7.5
    reads_1 reads_2
    / Velvet.contigs
  in
  let cisa_assembly : fasta workflow =
    Cisa.merge [
      "SPAdes", spades_assembly ;
      "IDBA", idba_ud_assembly ;
      "Velvet", velvet_assembly ;
    ]
    |> Cisa.cisa genome_size
  in
  let reapr_spades = Reapr.reapr (reads_1, reads_2) spades_assembly in
  let quast_comparison =
    Quast.quast
      ~reference
      ~labels:["SPAdes" ; "IDBA-UD" ; "Velvet" ; "CISA" ; "SPAdes+REAPR"]
      [
        spades_assembly ;
        idba_ud_assembly ;
        velvet_assembly ;
        cisa_assembly ;
        reapr_spades / Reapr.assembly ;
      ]
  in
  let open Bistro_app in
  let rep x = "output" :: name :: x in
  [
    rep [ "SPAdes" ; "contigs.fa"] %> spades_assembly ;
    rep [ "IDBA" ; ] %> idba_ud_assembly ;
    rep [ "Velvet" ; ] %> velvet_assembly ;
    rep [ "CISA" ; ] %> cisa_assembly ;
    rep [ "reapr" ; "SPAdes" ] %> reapr_spades ;
    rep [ "quast" ] %> quast_comparison ;
  ]

let sequencer n fa =
  let ao =
    Arts.(
      art_illumina
        ~aln_output:False
        ~sam_output:False
        ~errfree_sam_output:False
        (Paired_end { len = 150 ;
                      mflen = 400. ;
                      sdev = 20. ;
                      matepair = false })
        (`Read_count n) fa
    )
  in
  (ao / Arts.pe_fastq `One,
   ao / Arts.pe_fastq `Two)


let bsubtilis_genome : fasta workflow =
  Unix_tools.wget "ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/bacteria/Bacillus_subtilis/representative/GCF_000227465.1_ASM22746v1/GCF_000227465.1_ASM22746v1_genomic.fna.gz"
  |> Unix_tools.gunzip

let bsubtilis = {
  name = "B.subtilis" ;
  genome_size = 5_000_000 ;
  reference = bsubtilis_genome ;
  reads = sequencer 100_000 bsubtilis_genome ;
}

let ecoli_genome : fasta workflow =
  Unix_tools.wget "http://www.ncbi.nlm.nih.gov/nuccore/49175990?report=fasta"

let ecoli_articial = {
  name = "E.coli (artificial)" ;
  genome_size = 4639675 ;
  reference = ecoli_genome ;
  reads = sequencer 100_000 ecoli_genome
}

let ecoli = {
  name = "E.coli (artificial)" ;
  genome_size = 4639675 ;
  reference = ecoli_genome ;
  reads = (fetch_fq_gz "http://spades.bioinf.spbau.ru/spades_test_datasets/ecoli_mc/s_6_1.fastq.gz",
           fetch_fq_gz "http://spades.bioinf.spbau.ru/spades_test_datasets/ecoli_mc/s_6_3.fastq.gz") ;
}

let whole_pipeline () =
  List.concat [
    pipeline bsubtilis ;
    pipeline ecoli_articial ;
    pipeline ecoli ;
  ]

let main preview_mode outdir np mem () =
  let term = Bistro_app.of_repo ~outdir (whole_pipeline ()) in
  Bistro_app.run ~np ~mem:(mem * 1024) term

let spec =
  let open Command.Spec in
  empty
  +> flag "--preview-mode" no_arg ~doc:" Run on a small subset of the data"
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"

let command =
  Command.basic
    ~summary:"Genome assembler benchmark for prokaryotes"
    spec
    main
