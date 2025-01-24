type etat_balle = (float * float) * (float * float)
type rect = (float * float) * (float * float)
type ball = (float * float) * float
val mouse : 'a
val is_colliding :
  ('a * 'b) * 'c -> ('d * 'e) * ('f * 'g) -> 'h * 'i -> float * float
val integre : 'a -> 'b -> 'c
module Collision :
  sig
    val dt : 'a
    val contact_x : 'a -> 'b -> bool
    val rebond_x : 'a -> 'b -> 'b
    val contact_y : 'a -> 'b -> bool
    val rebond_y : 'a -> 'b -> 'b
  end
