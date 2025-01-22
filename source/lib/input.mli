open Iterator

val mouse : (float * bool) flux

(* le type des états de la forme (x, y), (dx, dy)  *)
(* i.e. position (x, y) et vitesse (dx, dy)        *)
type etat = (float * float) * (float * float)

type rect = (float*float)*(float*float)
type ball = (float*float)*float

val get_origin : rect -> (float*float)
val get_extremum : rect -> (float*float)
val get_size : rect -> (float*float)

val get_center : ball -> (float*float)
val get_radius : rect -> float

module type Collision = 
  sig
  val dt : float

  val contact_x : float -> float -> bool (* Args : pos_x, dx -> Result : bool *)
  val contact_y : float -> float -> bool (* Args : pos_x, dx -> Result : bool *)
  val rebond_x : float -> float -> bool (* Args : pos_x, dx -> Result : bool *)
  val rebond_y : float -> float -> bool (* Args : pos_x, dx -> Result : bool *)

  val unless : 'a flux -> ('a -> bool) -> ('a -> 'a flux) -> 'a flux

  val run : etat -> etat Flux.t

  end