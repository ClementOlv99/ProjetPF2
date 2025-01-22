open LibNewtonoid

(*position et vitesse de la balle*)
type balle = (float * float) * (float * float) 

(*abcisse de la souris*)
type raquette = float 

(*score + nb de vie*)
type score = int * int

(*etat de la partie*)
type etat = balle * raquette * score * (Quadtree.tree * int)

let game_init liste_brique = 

  let balle = ((Box.sup_x/.2., Box.sup_y/.2.), (0., 0.)) in

  let raquette = mouse in

  let score = 0, 3 in

  let quadtree = Quadtree.create_tree (Box.sup_x, Box.sup_y/2) liste_brique in

  balle, raquette, score, (quadtree, List.length liste_brique)

  