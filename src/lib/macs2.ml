open Workflow.Types
open Workflow.API

let package_script = Utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/macs2-install/2.1.0.20140616/macs2-install.sh"

let package =
  workflow [
    bash package_script [ target () ]
  ]

let macs2 cmd opts =
  program "macs2" ~path:[package] ~pythonpath:[package] (string cmd :: opts)

let pileup ?extsize ?both_direction bam =
  workflow [
    macs2 "pileup" [
      opt "-i" dep bam ;
      opt "-o" target () ;
      option (flag string "-B") both_direction ;
      option (opt "--extsize" int) extsize ;
    ]
  ]
