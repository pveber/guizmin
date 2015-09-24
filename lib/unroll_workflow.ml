open Core.Std
open Bistro
open Bistro_std
open Bistro_std.Types
open Unrolled_workflow

let unsafe_file_of_url url : 'a workflow =
  let source () =
    if String.is_prefix ~prefix:"http://" url || String.is_prefix ~prefix:"ftp://" url
    then Unix_utils.wget url
    else Workflow.input url
  in
  if Filename.check_suffix url ".gz"
  then Unix_utils.gunzip (source ())
  else source ()


module type Settings =
  sig
    val config_file : Experiment_description.t
  end

module Make(S : Settings) = struct
  open Experiment_description
  open Option.Monad_infix

  let extract f = List.filter_map S.config_file ~f
  let extract_unique f = List.dedup (extract f)

  let project_name =
    List.find_map S.config_file ~f:(function
        | Project name -> Some name
        | _ -> None
      )
    |>
    Option.value ~default:"X"

  let factors = extract_unique (
    function
    | Factor f -> Some f
    | _ -> None
  )

  let factor name = List.find_exn factors ~f:(fun f -> f.factor_name = name)


  module Genome = struct
    type t = genome

    let list = extract_unique (
        function
        | Model m -> m.model_genome
        | _ -> None
      )

    let sequence = function
      | `ucsc x -> Ucsc_gb.genome_sequence x
      | `fasta url -> unsafe_file_of_url url

    let bowtie_index g =
      Bowtie.bowtie_build (sequence g)

    let bowtie2_index g =
      Bowtie2.bowtie2_build (sequence g)

  end

  module Model = struct
    type t = model

    let list = extract_unique (
        function
        | Model m -> Some m
        | _ -> None
      )

    let annotation x = x.model_annotation

    let gene_annotation x =
      annotation x >>| function
      | `ensembl (species, release) ->
        Ensembl.gff ~chr_name:`ucsc ~release ~species
      | `gff gff_url ->
        unsafe_file_of_url gff_url
  end

  module Sample = struct
    type t = sample

    let list = extract_unique (
        function
        | Sample s -> Some s
        | _ -> None
      )

    let model s = List.find_exn Model.list ~f:(fun m -> m.model_id = s.sample_model)

    let genome s = (model s).model_genome

    let condition s =
      List.map s.sample_condition ~f:(fun (fn,fv) -> factor fn, fv)

    let ucsc_genome s = genome s >>= function
      | `ucsc org -> Some org
      | `fasta _ -> None

    let short_read_data s = match s.sample_data with
      | `short_read_data data -> Some data

    let sanger_fastq s =
      short_read_data s >>|
      function
      | `fastq (encoding, se_or_pe) ->
        let f urls =
          let g x = List.map urls ~f:(fun url -> Fastq.to_sanger x (unsafe_file_of_url url)) in
          match encoding with
          | `sanger -> g Fastq.Sanger
          | `solexa -> g Fastq.Solexa
          | `phred64 -> g Fastq.Phred64
        in
        se_or_pe_map se_or_pe ~f
      | `sra (se_or_pe, sra) ->
        let sras = match sra with
          | `SRR ids -> List.map ids ~f:Sra.fetch_srr
          | `file urls -> List.map urls ~f:unsafe_file_of_url
        in
        match se_or_pe with
        | `single_end -> `single_end (List.map sras ~f:Sra_toolkit.fastq_dump)
        | `paired_end -> assert false

    let fastQC_report s =
      sanger_fastq s >>| fun fqs ->
      let f x = FastQC.run (Fastq.concat x) in
      se_or_pe_map fqs ~f


    let dna_seq_mapped_reads_sam s genome fqs =
      let index = Genome.bowtie_index genome in
      Bowtie.bowtie ~v:2 ~m:1 index fqs

    let dna_seq_mapped_reads_indexed s genome fqs =
      Samtools.indexed_bam_of_sam (dna_seq_mapped_reads_sam s genome fqs)

    let dna_seq_mapped_reads s genome fqs =
      Samtools.bam_of_indexed_bam (dna_seq_mapped_reads_indexed s genome fqs)

    let tophat s genome fqs =
      let index = Genome.bowtie2_index genome in
      Tophat.tophat2 index fqs

    let mrna_seq_mapped_reads_indexed s genome fqs =
      Samtools.indexed_bam_of_bam (Tophat.accepted_hits (tophat s genome fqs))

    let mrna_seq_mapped_reads s genome fqs =
      Samtools.bam_of_indexed_bam (mrna_seq_mapped_reads_indexed s genome fqs)

    let mrna_seq_mapped_reads_sam s genome fqs =
      Samtools.sam_of_bam (mrna_seq_mapped_reads s genome fqs)


    let mapped_reads s =
      genome s >>= fun g_s ->
      sanger_fastq s >>= fun fqs ->
      match s.sample_exp with
      | `whole_cell_extract
      | `TF_ChIP _
      | `EM_ChIP _
      | `FAIRE ->
        Some (dna_seq_mapped_reads s g_s fqs)
      | `mRNA ->
        Some (mrna_seq_mapped_reads s g_s fqs)

    let mapped_reads_sam s =
      genome s >>= fun g_s ->
      sanger_fastq s >>= fun fqs ->
      match s.sample_exp with
      | `whole_cell_extract
      | `TF_ChIP _
      | `EM_ChIP _
      | `FAIRE ->
        Some (dna_seq_mapped_reads_sam s g_s fqs)
      | `mRNA ->
        Some (mrna_seq_mapped_reads_sam s g_s fqs)

    let mapped_reads_indexed s =
      genome s >>= fun g_s ->
      sanger_fastq s >>= fun fqs ->
      match s.sample_exp with
      | `whole_cell_extract
      | `TF_ChIP _
      | `EM_ChIP _
      | `FAIRE ->
        Some (dna_seq_mapped_reads_indexed s g_s fqs)
      | `mRNA ->
        Some (mrna_seq_mapped_reads_indexed s g_s fqs)

    let signal s =
      ucsc_genome s >>= fun org ->
      mapped_reads s >>| fun bam ->
      Ucsc_gb.bedGraphToBigWig org (Macs2.pileup bam)

    let chIP_TF s = match s.sample_exp with
      | `TF_ChIP tf -> Some tf
      | _ -> None

    let macs2_gsize_of_ucsc_org = function
      | `hg18 | `hg19         -> `hs
      | `mm8  | `mm9  | `mm10 -> `mm
      | `dm3                  -> `dm
      | `sacCer2              -> `gsize 12_000_000

    let macs2_peak_calling s =
      chIP_TF s >>= fun _ -> (* just to make sure we're dealing with TF ChIP samples *)
      ucsc_genome s >>= fun org ->
      mapped_reads s >>| fun bam ->
      Macs2.callpeak
        ~gsize:(macs2_gsize_of_ucsc_org org)
        ~qvalue:0.01
        ~call_summits:true
        ~fix_bimodal:true
        ~extsize:200
        bam

    let peak_calling s =
      macs2_peak_calling s >>| Macs2.narrow_peaks

    let read_counts_per_gene s =
      Model.gene_annotation (model s) >>= fun gff ->
      mapped_reads s >>| fun bam ->
      let bam = Samtools.sort ~on:`name bam in
      Htseq.count ~stranded:`no ~order:`name (`bam bam) gff
  end

  module Condition = struct
    type t = (factor * string) list

    let list =
      List.map Sample.list ~f:Sample.condition
      |> List.dedup

    let rec product = function
      | [] -> []
      | h :: t ->
        List.map t ~f:(fun i -> h, i)
        @
        product t

    let pairs = product list
  end

  module Transcriptome = struct

    let deseq2_wrapper_output =
      let samples =
        List.map Sample.list ~f:(fun s ->
            Sample.read_counts_per_gene s >>| fun counts ->
            s, counts
          )
        |> List.filter_opt
      in
      match samples with
      | [] | [ _ ] -> None
      | _ ->
        let factors =
          List.map factors ~f:(fun x -> x.factor_name)
          |> List.sort ~cmp:compare
        in
        let samples = List.map samples ~f:(fun (s, counts) ->
            let factors_s =
              s.sample_condition
              |> List.sort ~cmp:compare
              |> List.map ~f:snd
            in
            factors_s, counts
          )
        in
        Some (Deseq2.wrapper factors samples)
  end

  (* class tf_chip_seq_sample sample data genome tf = *)
  (*   object (s) *)
  (*     inherit simply_mapped_dna_seq_sample sample data genome *)
  (*     method tf : string = tf *)
  (*   end *)

  (* let samples = *)
  (*   extract_unique ( *)
  (*     function *)
  (*     | Sample s -> Some s *)
  (*     | _ -> None *)
  (*   ) *)


  (* let short_read_sample sobj data = *)
  (*   let s = sobj # repr in *)
  (*   match sobj # model . model_genome with *)
  (*   | Some g -> ( *)
  (*       match s.sample_exp with *)
  (*       | `TF_ChIP tf -> *)
  (*         `TF_ChIP_seq (new tf_chip_seq_sample s data g tf) *)
  (*       | `FAIRE -> *)
  (*         `FAIRE_seq (new simply_mapped_dna_seq_sample s data g) *)
  (*       | `EM_ChIP _ -> *)
  (*         `EM_ChIP_seq (new simply_mapped_dna_seq_sample s data g) *)
  (*       | `mRNA -> `mRNA_seq (new short_read_sample s data) *)
  (*       | `whole_cell_extract -> `WCE_seq (new simply_mapped_dna_seq_sample s data g) *)
  (*     ) *)
  (*   | None -> *)
  (*     `Short_read_sample (new short_read_sample s data) *)


  (* let any_samples = List.map samples ~f:(fun s -> any_sample (new sample s)) *)

  (* let tf_chip_seq_samples = List.filter_map any_samples ~f:(function *)
  (*     | `TF_ChIP_seq s -> Some s *)
  (*     | `EM_ChIP_seq _ *)
  (*     | `FAIRE_seq _ *)
  (*     | `mRNA_seq _ *)
  (*     | `WCE_seq _ *)
  (*     | `Short_read_sample _ -> None *)
  (*   ) *)

  (* let faire_seq_samples = List.filter_map any_samples ~f:(function *)
  (*     | `FAIRE_seq s -> Some s *)
  (*     | `TF_ChIP_seq _ *)
  (*     | `EM_ChIP_seq _ *)
  (*     | `mRNA_seq _ *)
  (*     | `WCE_seq _ *)
  (*     | `Short_read_sample _ -> None *)
  (*   ) *)

  (* let mappable_short_read_samples = List.filter_map any_samples ~f:(function *)
  (*     | `TF_ChIP_seq s -> Some (s :> mappable_short_read_sample) *)
  (*     | `EM_ChIP_seq s -> Some (s :> mappable_short_read_sample) *)
  (*     | `FAIRE_seq s -> Some (s :> mappable_short_read_sample) *)
  (*     | `mRNA_seq s -> None *)
  (*     | `WCE_seq s -> Some (s :> mappable_short_read_sample) *)
  (*     | `Short_read_sample s -> None *)
  (*   ) *)

  (* let short_read_samples = List.filter_map any_samples ~f:(function *)
  (*     | `TF_ChIP_seq s -> Some (s :> short_read_sample) *)
  (*     | `EM_ChIP_seq s -> Some (s :> short_read_sample) *)
  (*     | `FAIRE_seq s -> Some (s :> short_read_sample) *)
  (*     | `mRNA_seq s -> Some (s :> short_read_sample) *)
  (*     | `WCE_seq s -> Some (s :> short_read_sample) *)
  (*     | `Short_read_sample s -> Some (s :> short_read_sample) *)
  (*   ) *)

  (* let sample_of_any = function *)
  (*   | `TF_ChIP_seq s -> (s :> sample) *)
  (*   | `EM_ChIP_seq s -> (s :> sample) *)
  (*   | `FAIRE_seq s -> (s :> sample) *)
  (*   | `WCE_seq s -> (s :> sample) *)
  (*   | `mRNA_seq s -> (s :> sample) *)
  (*   | `Short_read_sample s -> (s :> sample) *)

  (* let samples = List.map any_samples ~f:sample_of_any *)

  (* let conditions = *)
  (*   List.map samples ~f:(fun s -> s#condition) *)
  (*   |> List.dedup *)
end

let from_description ged =
  let module X = struct let config_file = ged end in
  let module W = Make(X) in
  (module W : Unrolled_workflow.S)
