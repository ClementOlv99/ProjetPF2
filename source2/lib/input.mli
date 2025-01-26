type vector = float * float
(** Vecteur de dimension 2
    *)

type etat_balle = vector * vector
(** État de la balle, contient position et vitesse
    *)

type rect = vector * vector
(** Rectangle, contient position et taille
    *)

type ball = vector * float
(** Balle, contient centre et rayon
    *)

val dot : vector -> vector -> float
(** Produit scalaire de deux vecteurs.
    *)

val sub : vector -> vector -> vector
(** Soustraction de deux vecteurs
    *)

val scale : vector -> float -> vector
(** Produit d'un vecteur v et d'un scalaire s
    *)

val invert : vector -> vector
(** Inversion d'un vecteur v, shorthand de `scale v -1.0`
    *)

val div : vector -> float -> vector
(** Divise un vecteur v par un scalaire s, shorthand de `scale v 1/.s`
    *)

val norm : vector -> float
(** Norme 2 d'un vecteur
    *)

val proj : vector -> vector -> vector
(** Projection d'un vecteur v sur un vecteur p
    *)

val mirror : vector -> vector -> vector
(** Reflection d'un vecteur v par un vecteur m
    *)

val to_string : vector -> string
(** DEBUGGING. Convertit un vecteur en string.
    *)

val is_colliding : ball -> rect -> vector -> vector
(** Calcul des collisions

    Arguments :
    - balle : ball = balle du jeu
    - rectangle : rect = brique à tester
    - vitesse : vector = vitesse de la brique

    Renvoie : la nouvelle vitesse de la balle après collision si collision, (0.0,0.0) sinon
    *)
  
val integre : float -> vector Iterator.flux -> vector Iterator.flux
(** Intègre un flux

    Arguments :
    - dt : float = pas de temps
    - flux : vector Iterator.flux = flux à intégrer

    Renvoie : le flux intégré
    *)

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
