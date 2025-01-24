type etat_balle = (float * float) * (float * float)
type rect = (float * float) * (float * float)
type ball = (float * float) * float
val is_colliding :
  (float * float) * float ->
  (float * float) * (float * float) -> float * float -> float * float
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
