module type S = sig
  open Bistro_workflow.Types
  open Experiment_description

  val conditions : condition list
  val samples : sample list

  module Genome : sig
    type t = private genome
    val list : t list
    val sequence : t -> Fasta.workflow
  end

  module Short_read_sample : sig
    class type t = object
		     method sample : sample
		     method format : short_read_format
		   end
    val list : t list

    val sanger_fastq : #t -> [`sanger] Fastq.workflow list
    val fastQC_report : #t -> FastQC.workflow list
  end

  module DNA_seq_with_reference : sig
    type exp = [ `TF_ChIP of string | `FAIRE | `whole_cell_extract]
    class type [+'a] t = object
			  constraint 'a = [< exp]
			  inherit Short_read_sample.t
			  method experiment : 'a
			  method genomic_reference : Genome.t
			 end
    val list : exp t list

    val aligned_reads : 'a #t -> Sam.workflow
    val aligned_reads_indexed_bam : 'a #t -> [ `indexed_bam ] directory workflow
    val aligned_reads_bam : 'a #t -> Bam.workflow
  end

  module FAIRE_seq : sig
    type t = [`FAIRE] DNA_seq_with_reference.t

  end

end
