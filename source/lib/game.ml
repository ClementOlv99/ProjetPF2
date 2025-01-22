open Quadtree
open Iterator
open Input
open Init
open Dessin

(*etat de la partie*)
type etat = balle * raquette * score * (Quadtree.tree * int)

let game_init liste_brique = 

  let balle = ((Box.supx/.2., Box.supy/.2.), (0., 0.)) in

  let raquette = mouse in

  let score = 0, 3 in

  let quadtree = Quadtree.create_tree (Box.supx, Box.supy/2) liste_brique in

  (balle, raquette, score, (quadtree, List.length liste_brique))




    






