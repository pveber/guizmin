open Workflow.Types
open Ucsc_gb.Types
open Experiment_description

type condition = (factor * string) list

class type genome = object
  method repr : Experiment_description.genome
  method sequence : Fasta.workflow
  method bowtie_index : Bowtie.index workflow
end

class type sample = object
  method repr : Experiment_description.sample
  method id : string
  method data : sample_data
  method experiment : experiment
  method model : model
  method condition : condition
end

class type short_read_sample = object
  inherit sample
  method short_read_data : short_read_data
  method sanger_fastq  : [`sanger] Fastq.workflow list se_or_pe
  method fastQC_report : FastQC.workflow se_or_pe
end

class type mappable_short_read_sample = object
  inherit short_read_sample
  method reference_genome : genome
  method aligned_reads : Sam.workflow
  method aligned_reads_indexed_bam : [ `indexed_bam ] directory workflow
  method aligned_reads_bam : Bam.workflow
end

class type ucsc_short_read_sample = object
  inherit mappable_short_read_sample
  method ucsc_genome : Ucsc_gb.genome
  method signal : bedGraph workflow
end

class type tf_chip_seq_sample = object
  inherit mappable_short_read_sample
  method tf : string
end

type any_sample = [
  | `TF_ChIP_seq of tf_chip_seq_sample
  | `EM_ChIP_seq of mappable_short_read_sample
  | `FAIRE_seq of mappable_short_read_sample
  | `WCE_seq of mappable_short_read_sample
  | `mRNA_seq of short_read_sample
  | `Short_read_sample of short_read_sample
]

module type S = sig

  val project_name : string

  val conditions : condition list
  val genomes : genome list


  val sample_of_any : any_sample -> sample
  val any_sample : sample -> any_sample

  val any_samples : any_sample list

  val samples : sample list

  val short_read_samples : short_read_sample list

  val mappable_short_read_samples : mappable_short_read_sample list

  val tf_chip_seq_samples : tf_chip_seq_sample list

  val faire_seq_samples : mappable_short_read_sample list

end

