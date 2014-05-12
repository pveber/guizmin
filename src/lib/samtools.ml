open Bistro_workflow.Types

let package = Bistro_workflow.make <:script<
  URL=http://sourceforge.net/projects/samtools/files/samtools/0.1.19/samtools-0.1.19.tar.bz2/download
  ARCHIVE=samtools-0.1.19.tar.bz2
  PACKAGE=samtools-0.1.19

  cd #TMP
  wget -O $ARCHIVE $URL
  tar xvfj $ARCHIVE
  cd $PACKAGE
  make
  mkdir -p #DEST/bin
  mkdir -p #DEST/include/bam
  mkdir -p #DEST/lib
  cp samtools #DEST/bin
  cp *.h #DEST/include/bam
  cp libbam.a #DEST/lib
>>

let sam_of_bam bam =
  Bistro_workflow.make <:script<
    samtools view -o #DEST #w:bam#
  >>

let indexed_bam_of_sam sam = Bistro_workflow.make <:script<
  mkdir -p #DEST
  samtools view -S -b -o #DEST/temp.bam #w:sam#
  samtools sort #DEST/temp.bam #DEST/reads
  samtools index #DEST/reads.bam
  rm #DEST/temp.bam
>>

let bam_of_indexed_bam ibam = Bistro_workflow.select ibam "reads.bam"
