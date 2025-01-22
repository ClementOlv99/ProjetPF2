open Box 
open Quadtree




(*position et vitesse de la balle*)
type balle = (float * float) * (float * float) 

(*abcisse de la souris*)
type raquette = float 

(*score + nb de vie*)
type score = int * int

(*etat de la partie*)
type etat = balle * raquette * score * (Quadtree.tree * int)