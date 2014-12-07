open Core.Std
open Workflow.Types
open Unrolled_workflow

let unsafe_file_of_url url : 'a workflow =
  let source () =
    if String.is_prefix ~prefix:"http://" url || String.is_prefix ~prefix:"ftp://" url
    then Utils.wget url
    else Workflow.input url
  in
  if Filename.check_suffix url ".gz"
  then Utils.gunzip (source ())
  else source ()


module type Settings =
  sig
    val config_file : Experiment_description.t
  end

module Make(S : Settings) = struct
  open Experiment_description

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

  let models =
    extract_unique (
      function
      | Model m -> Some m
      | _ -> None
    )

  let model id = List.find_exn models ~f:(fun m -> m.model_id = id)

  class genome g = object (s)
    method repr : Experiment_description.genome = g
    method sequence = match g with
      | `ucsc x -> Ucsc_gb.genome_sequence x
      | `fasta url -> unsafe_file_of_url url
    method bowtie_index =
      Bowtie.bowtie_build s#sequence
  end

  let genomes = extract_unique (
      function
      | Model m -> Option.map ~f:(new genome) m.model_genome
      | _ -> None
    )

  let genome g = List.find_exn genomes ~f:(fun x -> x # repr = g)

  class sample (s : Experiment_description.sample) = object
    method repr = s
    method id = s.sample_id
    method data = s.sample_data
    method experiment = s.sample_exp
    method model = model s.sample_model
    method condition =
      List.map s.sample_condition ~f:(fun (fn,fv) -> factor fn, fv)
  end

  let sanger_fastq_of_short_read_data = function
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
      | `single_end -> `single_end (List.map sras ~f:Sra.fastq_dump)
      | `paired_end -> assert false

  class short_read_sample sample data : Unrolled_workflow.short_read_sample = object (s)
    inherit sample sample
    method short_read_data : short_read_data = data
    method sanger_fastq = sanger_fastq_of_short_read_data data
    method fastQC_report =
      let f x = FastQC.run (Fastq.concat x) in
      se_or_pe_map s#sanger_fastq ~f
  end

  class short_read_sample_with_reference_genome sample data g = object
    inherit short_read_sample sample data
    method reference_genome = genome g
  end

  class simply_mapped_dna_seq_sample sample data genome =
    object (s)
      inherit short_read_sample_with_reference_genome sample data genome

      method aligned_reads =
        let index = s#reference_genome#bowtie_index in
        Bowtie.bowtie ~v:2 ~m:1 index s#sanger_fastq

      method aligned_reads_indexed_bam =
        Samtools.indexed_bam_of_sam s#aligned_reads

      method aligned_reads_bam =
        Samtools.bam_of_indexed_bam s#aligned_reads_indexed_bam
    end

  class tf_chip_seq_sample sample data genome tf =
    object (s)
      inherit simply_mapped_dna_seq_sample sample data genome
      method tf : string = tf
    end

  let samples =
    extract_unique (
      function
      | Sample s -> Some s
      | _ -> None
    )


  let short_read_sample sobj data =
    let s = sobj # repr in
    match sobj # model . model_genome with
    | Some g -> (
        match s.sample_exp with
        | `TF_ChIP tf ->
          `TF_ChIP_seq (new tf_chip_seq_sample s data g tf)
        | `FAIRE ->
          `FAIRE_seq (new simply_mapped_dna_seq_sample s data g)
        | `EM_ChIP _ ->
          `EM_ChIP_seq (new simply_mapped_dna_seq_sample s data g)
        | `mRNA -> `mRNA_seq (new short_read_sample s data)
        | `whole_cell_extract -> `WCE_seq (new simply_mapped_dna_seq_sample s data g)
      )
    | None ->
      `Short_read_sample (new short_read_sample s data)

  let any_sample s : any_sample = match s # data with
    | `short_read_data data -> short_read_sample s data

  let any_samples = List.map samples ~f:(fun s -> any_sample (new sample s))

  let tf_chip_seq_samples = List.filter_map any_samples ~f:(function
      | `TF_ChIP_seq s -> Some s
      | `EM_ChIP_seq _
      | `FAIRE_seq _
      | `mRNA_seq _
      | `WCE_seq _
      | `Short_read_sample _ -> None
    )

  let faire_seq_samples = List.filter_map any_samples ~f:(function
      | `FAIRE_seq s -> Some s
      | `TF_ChIP_seq _
      | `EM_ChIP_seq _
      | `mRNA_seq _
      | `WCE_seq _
      | `Short_read_sample _ -> None
    )

  let mappable_short_read_samples = List.filter_map any_samples ~f:(function
      | `TF_ChIP_seq s -> Some (s :> mappable_short_read_sample)
      | `EM_ChIP_seq s -> Some (s :> mappable_short_read_sample)
      | `FAIRE_seq s -> Some (s :> mappable_short_read_sample)
      | `mRNA_seq s -> None
      | `WCE_seq s -> Some (s :> mappable_short_read_sample)
      | `Short_read_sample s -> None
    )

  let short_read_samples = List.filter_map any_samples ~f:(function
      | `TF_ChIP_seq s -> Some (s :> short_read_sample)
      | `EM_ChIP_seq s -> Some (s :> short_read_sample)
      | `FAIRE_seq s -> Some (s :> short_read_sample)
      | `mRNA_seq s -> Some (s :> short_read_sample)
      | `WCE_seq s -> Some (s :> short_read_sample)
      | `Short_read_sample s -> Some (s :> short_read_sample)
    )

  let sample_of_any = function
    | `TF_ChIP_seq s -> (s :> sample)
    | `EM_ChIP_seq s -> (s :> sample)
    | `FAIRE_seq s -> (s :> sample)
    | `WCE_seq s -> (s :> sample)
    | `mRNA_seq s -> (s :> sample)
    | `Short_read_sample s -> (s :> sample)

  let samples = List.map any_samples ~f:sample_of_any

  let conditions =
    List.map samples ~f:(fun s -> s#condition)
    |> List.dedup
end

let from_description ged =
  let module X = struct let config_file = ged end in
  let module W = Make(X) in
  (module W : Unrolled_workflow.S)

module Make_alt(S : Settings) = struct
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

  end

  module Model = struct
    type t = model

    let list = extract_unique (
        function
        | Model m -> Some m
        | _ -> None
      )

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
        | `single_end -> `single_end (List.map sras ~f:Sra.fastq_dump)
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
        None

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
        None

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
        None

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

let from_description_alt ged =
  let module X = struct let config_file = ged end in
  let module W = Make_alt(X) in
  (module W : Unrolled_workflow.S_alt)
