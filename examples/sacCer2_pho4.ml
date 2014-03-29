(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

#require "guizmin_workflow"

open Guizmin_workflow

open Experiment_description

let chIP_pho4_noPi = {
  sample_id = "chIP_pho4_noPi" ;
  sample_type = `short_reads `sra ;
  sample_exp = `FAIRE ;
  sample_files = [
    "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/SRR217/SRR217304/SRR217304.sra" ;
    "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/SRR217/SRR217305/SRR217305.sra" ;
  ] ;
  sample_model = "sacCer" ;
  sample_condition = "noPi" ;
}

let noPi = "noPi"
let sacCer = {
  model_id = "sacCer" ;
  model_genome = `ucsc `sacCer2
}

let config = [
  Condition noPi ;
  Model sacCer ;
  Sample chIP_pho4_noPi ;
]

let () =
  Experiment_description.save config Sys.argv.(1)

