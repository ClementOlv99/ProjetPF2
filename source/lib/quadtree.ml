open Init

type coord = float * float

(* coordonnées des coins inférieur gauche et supérieur droit d'une case du quadtree *)
type limites = coord * coord


(** type du quadtree *)
type tree =
  | Leaf of limites * (coord option)
  | Node of limites * tree * tree * tree * tree

(******************************************************************************)
(*      fonction intermediaire de comparaison                                 *)
(*   signature :                                                              *)
(*		inside : (float * float) -> (float * float) * (float * float) -> bool *)
(*   paramètre(s) : les coordonnées de la balle						          *)
(*					les coordonnéesd des limites de l'espace				  *)
(*   résultat     : true si la balle est dans l'espace delimité               *)
(*			par les coordonnées, false sinon.                                 *)
(******************************************************************************)
let inside : coord -> limites -> bool =
	fun coord_balle intervalle ->
		let (xb, yb) = coord_balle in
		let ((xmin , xmax) , (ymin , ymax)) = intervalle in
		(xb <= xmin) || ((xb >= xmax) || ((yb <= ymin) || (yb >= ymax)))

(******************************************************************************)
(*      fonction intermediaire de calcul de norme                             *)
(*   signature :                                                              *)
(*		norme : (float * float) -> float                                      *)
(*   paramètre(s) : les coordonnées du vecteur						          *)
(*   résultat     : la norme du vecteur 	                                  *)
(******************************************************************************)
let norme : coord -> float =
	fun coord_vect ->
		let (xv, yv) = coord_vect in
		sqrt ((Float.pow xv 2.0) +. (Float.pow yv 2.0))


let predict : int -> coord -> (float * float) -> coord list =
	fun r coord_balle vitesse_balle ->
		let (xb, yb) = coord_balle in
		let (vx, vy) = vitesse_balle in
		let (dir_x, dir_y) = ((vx /. norme(vitesse_balle)), (vy /. norme(vitesse_balle))) in
		let (pred_x, pred_y) = (xb +. (r *. dir_x)) ,(yb +. (r *. dir_y)) in
		let a = r dir_x in a


		(*x + "r*(vx/norme(v))" + vx*)
