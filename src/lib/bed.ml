open Bistro_workflow.Types

type 'a bed3_like = (string * (int * (int * 'a)), [`no], [`sharp]) tsv

type bed3 = unit bed3_like

