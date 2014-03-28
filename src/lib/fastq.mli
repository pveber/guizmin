open Bistro_types

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

val sanger : [`sanger] format
val solexa : [`solexa] format
val phred64 : [`phred64] format

type 'a workflow = 'a format file Bistro_workflow.t

val sanger_of_solexa : [`solexa] workflow -> [`sanger] workflow
val sanger_of_solexa : [`phred64] workflow -> [`sanger] workflow
val to_sanger : 'a format -> 'a workflow -> [`sanger] workflow

(* val nbreads : 'a workflow -> int workflow *)
