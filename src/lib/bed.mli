open Workflow.Types

type 'a bed3_like = (string * (int * (int * 'a)), [`no], [`sharp]) tsv

type bed3 = unit bed3_like

type 'a bed4_like = (string * 'a) bed3_like

type bed4 = unit bed4_like
