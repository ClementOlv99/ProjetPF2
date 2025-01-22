open Iterator
(** S'occupe de la partie collisions et physiques du jeu.
    *)

val mouse : (float * bool) flux

(* le type des états de la forme (x, y), (dx, dy)  *)
(* i.e. position (x, y) et vitesse (dx, dy)        *)
type etat

type rect
(** Représente un rectangle.

    (x,y),(tx,ty) :
      - x : float = origine sur l'axe X 
      - y : float = origine sur l'axe Y
      - tx : float = taille sur l'axe X 
      - ty : float = taille sur l'axe Y 
    *)
type ball
(** Représente une balle (cercle).

    (x,y),r :
      - x : float = centre sur l'axe X 
      - y : float = centre sur l'axe Y
      - r : float = rayon
    *)

val is_colliding : ball -> rect -> (float*float) -> (float*float)
(** Vérifie si une balle et un rectangle sont en collision.
    
    Arguments : balle rectangle (vx,vy)
    - balle : ball = balle sujet de la collision
    - rectangle : rect = rectangle sujet de la collision
    - vx : float : vitesse de la balle sur l'axe X
    - vy : float : vitesse de la balle sur l'axe Y

    Résultat : (nx,ny)
      - nx : float = composante sur X de la normale de la collision
      - ny : float = composante sur Y de la normale de la collision
    
    Post-Conditions :
      - le vecteur (nx,ny) est normal
    *)

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