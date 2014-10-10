type t = statement list
and statement =
| Condition of condition
| Sample of sample
| Model of model
| Project of string
and condition = string
and sample = {
  sample_id : string ;
  sample_type : sample_type ;
  sample_exp : experiment ;
  sample_files : string list ;
  sample_model : string ;
  sample_condition : string ;
}
and sample_type = [
| `short_reads of short_read_format
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_format = [
| `fastq of [ `sanger | `solexa | `phred64 ]
| `sra
]
and model = {
  model_id : string ;
  model_genome : genome option ;
}
and genome = [
| `ucsc of Ucsc_gb.genome
| `fasta of string
]
with sexp

val load : string -> t
val save : t -> string -> unit

type error = [
  | `multiple_declaration of [`condition | `model | `sample] * string
  | `missing_project_description
  | `more_than_one_project_description of string list
]
with sexp

val check : t -> error list
val error_msg : error -> string

