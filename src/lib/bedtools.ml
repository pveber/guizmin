open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/bedtools-install/2.11.2/bedtools-install.sh"

let package =
  workflow [
    bash package_script [ dest ]
  ]

