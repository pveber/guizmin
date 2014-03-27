module type Settings =
  sig
    val config_file : Experiment_description.t
  end

module Make(S : Settings) :
sig
  open Experiment_description

  val conditions : condition list
  val samples : sample list
  val genomes : genome list
end

