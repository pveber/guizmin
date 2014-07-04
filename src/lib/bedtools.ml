open Bistro_workflow.Types

let package = Bistro_workflow.make <:script<

set -e
URL=http://bedtools.googlecode.com/files/BEDTools.v2.11.2.tar.gz
ARCHIVE=`basename ${URL}`

mkdir -p #TMP
(
  cd #TMP
  wget -O ${ARCHIVE} ${URL}
  tar xvfz ${ARCHIVE} --strip-components=1
  make
)
mkdir -p #DEST/bin
cp #TMP/bin/* #DEST/bin

>>

