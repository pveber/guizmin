open Bistro_std

type species = [
  | `homo_sapiens
  | `mus_musculus
]

val ucsc_reference_genome : release:int -> species:species -> [`mm9 | `hg19]

val gff : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> Gff.file workflow
