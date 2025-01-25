type vector = float * float
type etat_balle = vector * vector
type rect = vector * vector
type ball = vector * float
val dot : float * float -> float * float -> float
val sub : float * float -> float * float -> vector
val scale : float * float -> float -> vector
val invert : float * float -> vector
val div : float * float -> float -> vector
val norm : float * float -> float
val proj : float * float -> float * float -> vector
val mirror : float * float -> float * float -> vector
val to_string : float * float -> string
val is_colliding :
  (float * float) * float ->
  (float * float) * (float * float) -> float * float -> float * float
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
      float * float ->
      (float * float) list -> 'a * float -> (float * float) * (float * float)
  end
