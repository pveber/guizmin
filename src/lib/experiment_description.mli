type t = statement list
and statement =
| Condition of condition
| Sample of sample
| Model of model
| Project of string
and condition = string
and sample = {
  sample_id : string ;
  sample_data : sample_data ;
  sample_exp : experiment ;
  sample_model : string ;
  sample_condition : string ;
}
and sample_data = [
| `short_read_data of short_read_data
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `EM_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_data = [
| `fastq of
    [ `sanger | `solexa | `phred64 ] *
    string list se_or_pe
| `sra of
    [`single_end | `paired_end] *
    [ `SRR of string | `file of string ]
]
and model = {
  model_id : string ;
  model_genome : genome option ;
}
and genome = [
| `ucsc of Ucsc_gb.genome
| `fasta of string
]
and 'a se_or_pe = [
  | `single_end of 'a
  | `paired_end of 'a * 'a
]
with sexp

val load : string -> t
val save : t -> string -> unit

val se_or_pe_map : 'a se_or_pe -> f:('a -> 'b) -> 'b se_or_pe

type error = [
  | `undeclared of [`condition | `model | `sample] * string
  | `multiple_declaration of [`condition | `model | `sample] * string
  | `missing_project_description
  | `more_than_one_project_description of string list
]
with sexp

val check : t -> error list
val error_msg : error -> string

