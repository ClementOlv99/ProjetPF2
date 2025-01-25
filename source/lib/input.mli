
type vector = float*float
type etat_balle  = vector*vector
type rect = (vector) * (vector)
type ball = (vector) * float
val is_colliding :
  ball -> rect -> vector -> vector
val integre :
  float -> (float * float) Iterator.flux -> (float * float) Iterator.flux
module Collision :
  sig
    val dt : float
    val contact_x : float -> float -> bool
    val contact_y : float -> float -> bool
    val rebond_x : float -> float -> float
    val rebond_y : float -> float -> float
    val rebond :
      float * float ->
      float * float -> (float * float) list -> int * float -> float * float
  end
