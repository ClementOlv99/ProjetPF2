type tree = Noeud of ((float * float) * (float * float)) * (branche option  * (int * int) option)
and branche = (tree * tree * tree * tree)

let bind f ent flott = f flott (Float.of_int ent)

(******************************************************************************)
(*      fonction intermediaire de comparaison                                 *)
(*   signature :                                                              *)
(*		inside : (float * float) -> (float * float) * (float * float) -> bool *)
(*   paramètre(s) : les coordonnées de la balle						          *)
(*					les coordonnéesd des limites de l'espace				  *)
(*   résultat     : true si la balle est dans l'espace delimité               *)
(*			par les coordonnées, false sinon.                                 *)
(******************************************************************************)
let inside : (float * float) -> (float * float) * (float * float) -> bool =
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
let norm : (float * float) -> float =
	fun coordVect ->
		let (xv, yv) = coordVect in
		sqrt ((Float.pow xv 2.0) +. (Float.pow yv 2.0))


let predict : int -> (int*int) -> (float * float) -> (float * float) list =
	fun r coordBalle vitesseBalle ->
		let (xb, yb) = coordBalle in
		let (vx, vx) = vitesseBalle in
		(*x + "r*(vx/norme(v))" + vx*)
