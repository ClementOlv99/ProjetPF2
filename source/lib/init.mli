module Data : sig val dt : float end
module Box :
  sig
    val marge : float
    val infx : float
    val infy : float
    val supx : float
    val supy : float
  end
module TailleBriqueInit : sig val width : int val height : int end
module RaquetteInit : sig val width : int val height : int val ypos : int end
type balle = (float * float) * (float * float)
type raquette = float
type score = int * int