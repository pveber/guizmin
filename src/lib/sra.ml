open Core.Std
open Bistro_workflow.Types

type format

type workflow = ([`sra], [`binary]) file Bistro_workflow.t

let package = Bistro_workflow.make <:script<

MACHINE_TYPE=`uname -m`
if \[ ${MACHINE_TYPE} == 'x86_64' \]; then
  URL=http://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.1.16/sratoolkit.2.1.16-centos_linux64.tar.gz
  ARCHIVE=`basename ${URL}`
else
  URL=http://ftp-private.ncbi.nlm.nih.gov/sra/sdk/2.1.16/sratoolkit.2.1.16-ubuntu32.tar.gz
  ARCHIVE=`basename ${URL}`
fi ;

PACKAGE=${ARCHIVE%\.tar.gz}
mkdir -p #TMP
cd #TMP
wget -O ${ARCHIVE} ${URL}
tar xvfz ${ARCHIVE}
cd ${PACKAGE}
rm -rf USAGE README help
mkdir -p #DEST/bin
cp bin/* #DEST/bin

>>

let input x = Bistro_workflow.input x

let fetch_srr id =
  if (String.length id = 9) then (
    let prefix = String.prefix id 6 in
    Utils.wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id)
  )
  else failwithf "Guizmin_workflow.Sra.fetch_srr: id %s is invalid (not 9 characters long)" id ()

let fastq_dump sra = Bistro_workflow.make <:script<
  export PATH=$w:package$/bin:$PATH
  fastq-dump -Z #w:sra# > #DEST
>>














