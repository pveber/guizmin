open Bistro_workflow.Types
open Experiment_description

class type genome = object
  method repr : Experiment_description.genome
  method sequence : Fasta.workflow
  method bowtie_index : Bowtie.index workflow
end

class type sample = object
  method repr : Experiment_description.sample
  method id : string
  method _type : sample_type
  method experiment : experiment
  method model : model
  method condition : string
end

class type short_read_sample = object
  inherit sample
  method format : short_read_format
  method sanger_fastq : [`sanger] Fastq.workflow list
  method fastQC_report : FastQC.workflow list
end

class type mappable_short_read_sample = object
  inherit short_read_sample
  method reference_genome : genome
  method aligned_reads : Sam.workflow
  method aligned_reads_indexed_bam : [ `indexed_bam ] directory workflow
  method aligned_reads_bam : Bam.workflow
end

class type tf_chip_seq_sample = object
  inherit mappable_short_read_sample
  method tf : string
end

module type S = sig

  val conditions : condition list
  val genomes : genome list

  val samples : sample list

  val short_read_samples : short_read_sample list

  val mappable_short_read_samples : mappable_short_read_sample list

  val tf_chip_seq_samples : tf_chip_seq_sample list

  val faire_seq_samples : mappable_short_read_sample list

end
