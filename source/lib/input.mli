
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
    val rebond_x : float -> float -> float
    val contact_y : float -> float -> bool
    val rebond_y : float -> float -> float
  end
