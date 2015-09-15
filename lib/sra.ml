open Core.Std
open Bistro_std
open Bistro.EDSL_sh

type workflow = ([`sra], [`binary]) file Workflow.t

let toolkit_package = Workflow.make ~descr:"Sra.toolkit_package" [%sh{|
PREFIX={{DEST}}
TMP={{TMP}}

set -e

MACHINE_TYPE=`uname -m`
if [ ${MACHINE_TYPE} == 'x86_64' ]; then
    URL=http://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.4.2-1/sratoolkit.2.4.2-centos_linux64.tar.gz
    ARCHIVE=`basename ${URL}`
else
    URL=http://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.4.2-1/sratoolkit.2.4.2-ubuntu64.tar.gz
    ARCHIVE=`basename ${URL}`
fi ;
PACKAGE=${ARCHIVE%\.tar.gz}

cd $TMP
wget -O ${ARCHIVE} ${URL}
tar xvfz ${ARCHIVE}
cd ${PACKAGE}
rm -rf USAGE README help

mkdir -p ${PREFIX}/bin
cp bin/* ${PREFIX}/bin
|}]

let input x = Workflow.input x

let fetch_srr id =
  if (String.length id > 6) then (
    let prefix = String.prefix id 6 in
    Utils.wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id)
  )
  else failwithf "Guizmin_workflow.Sra.fetch_srr: id %s is invalid (should be longer than 6 characters long)" id ()

let fastq_dump sra =
  workflow ~descr:"sratoolkit.fastq_dump" [
    cmd "fastq-dump" ~path:[toolkit_package] [ string "-Z" ; dep sra ] ~stdout:dest
  ]

let fastq_dump_pe sra =
  let dir =
    workflow ~descr:"sratoolkit.fastq_dump" [
      mkdir_p dest ;
      cmd "fastq-dump" ~path:[toolkit_package] [
        opt "-O" ident dest ;
        string "--split-files" ;
        dep sra
      ] ;
      mv (dest // "*_1.fastq") (dest // "reads_1.fastq") ;
      mv (dest // "*_2.fastq") (dest // "reads_2.fastq") ;
    ]
  in
  Workflow.extract dir ["reads_1.fastq"],
  Workflow.extract dir ["reads_2.fastq"]














