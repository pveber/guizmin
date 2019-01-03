open Bistro
open Bistro_bioinfo
open Experiment_description

type condition = (factor * string) list

module type S = sig

  val project_name : string

  module Genome : sig
    type t = genome

    val list : t list

    val sequence : t -> fasta pworkflow
    val bowtie_index : t -> Bowtie.index pworkflow
    val bowtie2_index : t -> Bowtie2.index pworkflow
  end

  module Model : sig
    type t = model

    val list : t list
    val annotation : t -> annotation option
    val gene_annotation : t -> gff pworkflow option
  end

  module Sample : sig
    type t = sample

    val list : t list

    val model : t -> model
    val genome : t -> genome option
    val condition : t -> (factor * string) list

    val ucsc_genome : t -> Ucsc_gb.genome option

    (** Short read samples *)
    val short_read_data : t -> short_read_data option
    val sanger_fastq  : t -> sanger_fastq pworkflow list se_or_pe option
    val fastQC_report : t -> FastQC.report pworkflow se_or_pe option

    (** Short read samples with a reference genome *)
    val mapped_reads : t -> bam pworkflow option
    val mapped_reads_indexed : t -> [ `indexed_bam ] dworkflow option
    val mapped_reads_sam : t -> sam pworkflow option

    val signal : t -> Ucsc_gb.bigWig pworkflow option

    (** Peak calling stuff *)
    val chIP_TF : t -> string option
    val macs2_peak_calling : t -> Macs2.narrow_output pworkflow option
    val peak_calling : t -> Macs2.narrow_peaks pworkflow option

    (** mRNA-seq analysis *)
    val read_counts_per_gene : t -> Htseq.count_tsv pworkflow option
  end

  module Condition : sig
    type t = (factor * string) list
    val list : t list
    val pairs : (t * t) list
  end

  module Transcriptome : sig
    val deseq2 : Model.t -> DESeq2.output option
  end
end
