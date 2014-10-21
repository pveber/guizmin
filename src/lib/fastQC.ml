open Workflow.Types

open Workflow.API

let package_install_script = Utils.wget "fixme"

(* let package = Bistro_workflow.make <:script< *)

(* URL=http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v0.10.1.zip *)
(* ARCHIVE=`basename ${URL}` *)

(* mkdir -p #TMP *)
(* cd #TMP *)
(* wget -O ${ARCHIVE} ${URL} *)
(* unzip ${ARCHIVE} *)
(* cd FastQC *)

(* mkdir -p #DEST/local/fastqc *)
(* cp -r * #DEST/local/fastqc *)
(* chmod 755 #DEST/local/fastqc/fastqc *)

(* cd #DEST/bin *)
(* ln -s ../local/fastqc/fastqc . *)

(* >> *)

(* type report *)
(* type workflow = report directory Bistro_workflow.t *)

(* let run fq = Bistro_workflow.make <:script< *)

(* export PATH=$w:package$/bin:$PATH *)
(* mkdir -p #DEST *)
(* fastqc --outdir=#DEST #w:fq# *)
(* rm #DEST/*.zip *)
(* mv #DEST/*_fastqc/* #DEST *)
(* rmdir #DEST/*_fastqc *)

(* >> *)


(* let html_report dir = *)
(*   Bistro_workflow.select dir "fastqc_report.html" *)

(* let per_base_quality dir = *)
(*   Bistro_workflow.select dir "Images/per_base_quality.png" *)

(* let per_base_sequence_content dir = *)
(*   Bistro_workflow.select dir "Images/per_base_sequence_content.png" *)

