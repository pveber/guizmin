open Std

val meme_chip :
  ?meme_nmotifs:int ->
  ?meme_minw:int ->
  ?meme_maxw:int ->
  ?meme_p:int ->
  Fasta.workflow ->
  [`meme_chip_output] directory workflow
