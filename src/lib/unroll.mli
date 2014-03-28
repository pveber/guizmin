module type Settings =
  sig
    val config_file : Experiment_description.t
  end

module Make(S : Settings) :
sig
  open Experiment_description

  val conditions : condition list
  val samples : sample list

  module Genome : sig
    type t = private genome
    val list : t list
    val sequence : t -> Fasta.workflow
  end

  module Short_read_sample : sig
    type t = private sample * short_read_format
    val list : t list

    val short_read_format : t -> short_read_format
    val sanger_fastq : t -> [`sanger] Fastq.workflow list
    val fastQC_report : t -> FastQC.workflow list
  end
end

