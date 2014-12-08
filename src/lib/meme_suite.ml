open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/meme-install/4.9.1/meme-install.sh"

let package = workflow [
    bash package_script [ target () ]
  ]

let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw ?meme_p fa =
  workflow [
    program ~path:[package] "meme_chip" [
      option (opt "-meme-nmotifs" int) meme_nmotifs ;
      option (opt "-meme-minw" int) meme_minw ;
      option (opt "-meme-maxw" int) meme_maxw ;
      option (opt "-meme-p" int) meme_p ;
      dep fa ;
    ]
  ]
