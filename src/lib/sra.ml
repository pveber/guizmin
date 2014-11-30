open Core.Std
open Workflow.Types
open Workflow.API

type format

type workflow = ([`sra], [`binary]) file Workflow.t

let package_install_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/sra-toolkit-install/2.4.2-1/sra-toolkit-install.sh"

let package =
  workflow [
    bash package_install_script [ target () ]
  ]

let input x = Workflow.input x

let fetch_srr id =
  if (String.length id > 6) then (
    let prefix = String.prefix id 6 in
    Utils.wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id)
  )
  else failwithf "Guizmin_workflow.Sra.fetch_srr: id %s is invalid (should be longer than 6 characters long)" id ()

let fastq_dump sra =
  workflow [
    program "fastq-dump" ~path:[package] [ string "-Z" ; dep sra ] ~stdout:(target ())
  ]


(* open Core.Std *)
(* open Bistro_workflow.Types *)

(* type format *)

(* type workflow = ([`sra], [`binary]) file Bistro_workflow.t *)

(* let package = Bistro_workflow.make ~interpreter:`bash <:script< *)

(* URL=http://ftp-trace.ncbi.nlm.nih.gov/sra/sdk/2.3.5-2/sratoolkit.2.3.5-2-centos_linux64.tar.gz *)
(* ARCHIVE=`basename ${URL}` *)
(* PACKAGE=${ARCHIVE%\.tar.gz} *)
(* mkdir -p #TMP *)
(* ( *)
(*   cd #TMP *)
(*   wget -O ${ARCHIVE} ${URL} *)
(*   tar xvfz ${ARCHIVE} *)
(*   cd ${PACKAGE} *)
(*   rm -rf USAGE README help *)
(* ) *)
(* mkdir -p #DEST/bin *)
(* cp -r #TMP/${PACKAGE}/bin/* #DEST/bin *)

(* >> *)

(* let input x = Bistro_workflow.input x *)

(* let fetch_srr id = *)
(*   if (String.length id = 9) then ( *)
(*     let prefix = String.prefix id 6 in *)
(*     Utils.wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id) *)
(*   ) *)
(*   else failwithf "Guizmin_workflow.Sra.fetch_srr: id %s is invalid (not 9 characters long)" id () *)

(* let fastq_dump sra = Bistro_workflow.make <:script< *)
(*   export PATH=#w:package#/bin:$PATH *)
(*   fastq-dump -Z #w:sra# > #DEST *)
(* >> *)














