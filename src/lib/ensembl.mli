open Workflow.Types

type species = [
  | `homo_sapiens
  | `mus_musculus
] with sexp

val ucsc_reference_genome : release:int -> species:species -> [`mm9 | `hg19]

type gff = private Gff.file
val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gff workflow
