open Std
open Bistro.EDSL_sh

let package = Workflow.make [%sh{|
PREFIX={{ dest }}

URL=ftp://ftp.ebi.edu.au/pub/software/MEME/4.9.1/meme_4.9.1.tar.gz
ARCHIVE=`basename ${URL}`
PACKAGE=${ARCHIVE%\.tar.gz}

mkdir -p $PREFIX/src
cd $PREFIX/src
wget ${URL}
tar xvfz ${ARCHIVE}
rm $ARCHIVE
cd ${PACKAGE}
./configure --prefix=${PREFIX} --with-url="http://meme.nbcr.net/meme" --enable-build-libxml2 --enable-build-libxslt
make
make install
make clean
|}]

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw ?meme_p fa =
  workflow [
    cmd ~path:[package] "meme_chip" [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      option (opt "-meme-p" int) meme_p ;
      dep fa ;
    ]
  ]
