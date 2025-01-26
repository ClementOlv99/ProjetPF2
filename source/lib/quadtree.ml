open Init

type coord = float * float

(* coordonnées des coins inférieur gauche et supérieur droit d'une case du quadtree *)
type limites = coord * coord

(** type du quadtree *)
type quadtree =
  | Leaf of limites * (coord option)
  | Node of limites * quadtree * quadtree * quadtree * quadtree


(******************************************************************************)
(*      fonction interne de calcul de norme                                   *)
(*   signature :                                                              *)
(*      norme : coord -> float                                                *)
(*   paramètre(s) : les coordonnées du vecteur                                *)
(*   résultat     : la norme du vecteur                                       *)
(******************************************************************************)
let norme : coord -> float =
fun coord_vect ->
	let (xv, yv) = coord_vect in
	sqrt ((Float.pow xv 2.0) +. (Float.pow yv 2.0))

(******************************************************************************)
(*      fonction interne pour verifier l'égalité entre 2 points               *)
(*   signature :                                                              *)
(*      equal : coord -> coord -> bool                                        *)
(*   paramètre(s) : les coordonnées des 2 points à comparer                   *)
(*   résultat     : le booléen resultat                                       *)
(******************************************************************************)
let equal : coord -> coord -> bool =
fun (x_1, y_1) (x_2, y_2) ->
	(x_1 = x_2) && (y_1 = y_2)

(******************************************************************************)
(*      batterie de fonctions intenes pour obtenir les limites des            *)
(*                    sous-quadrants d'un espace                              *)
(*   signatures :                                                             *)
(*      quadrant_no : limites -> limites                                      *)
(*      quadrant_ne : limites -> limites                                      *)
(*      quadrant_se : limites -> limites                                      *)
(*      quadrant_so : limites -> limites                                      *)
(*   paramètre(s) : les limites du sous espace à découper                     *)
(*   résultat     : les limites du quadrant resultant                         *)
(******************************************************************************)
let quadrant_no : limites -> limites =
fun ((x_so, y_so),(x_ne, y_ne)) ->
	((x_so, (y_ne +. y_so) /. 2.0), ((x_ne +. x_so) /. 2.0, y_ne))
let quadrant_ne : limites -> limites =
fun ((x_so, y_so),(x_ne, y_ne)) ->
	(((x_ne +. x_so) /. 2.0, (y_ne +. y_so) /. 2.0), (x_ne, y_ne))
let quadrant_se : limites -> limites =
fun ((x_so, y_so),(x_ne, y_ne)) ->
	(((x_ne +. x_so) /. 2.0, y_so), (x_ne, (y_ne +. y_so) /. 2.0))
let quadrant_so : limites -> limites =
fun ((x_so, y_so),(x_ne, y_ne)) ->
	((x_so, y_so) , ((x_ne +. x_so) /. 2.0, (y_ne +. y_so) /. 2.0))

(******************************************************************************)
(*      fonction interne pour placer objet dans espace (quel quart?)          *)
(*   signature :                                                              *)
(*      placement : coord -> limites -> bool                                  *)
(*   paramètre(s) : les coordonnées de l'objet à étudier                      *)
(*                  les limites de l'espace à tester                          *)
(*   résultat     : un couple de booléen indiquant le quadrant dans lequel    *)
(*                  l'objet doit être placé:                                  *)
(*      |true,true   -> quadrant nord-est                                     *)
(*      |true,false  -> quadrant sud-est                                      *)
(*      |false,true  -> quadrant nord-ouest                                   *)
(*      |false,false -> quadrant sud-ouest                                    *)
(******************************************************************************)
let placement : coord -> limites -> (bool * bool) =
fun (x_obj, y_obj) ((x_so, y_so), (x_ne, y_ne)) ->
	(x_obj > ((x_so +. x_ne) /. 2.0), y_obj > ((y_so +. y_ne) /. 2.0))


let predict : float -> ((float * float) * (float * float)) -> (float*float) list =
fun r ((xb, yb), vitesse_balle) ->
	let (vx, vy) = vitesse_balle in
	let (dir_x, dir_y) = ((vx /. norme(vitesse_balle)), (vy /. norme(vitesse_balle))) in
	let (pred_x, pred_y) = (xb +. (r *. dir_x)) ,(yb +. (r *. dir_y)) in
		(pred_x +. (dir_x*.r) , pred_y +. (dir_y*.r)) ::((pred_x +. (dir_y*.r) , pred_y -. (dir_x*.r)) :: ((pred_x -. (dir_y*.r) , pred_y +. (dir_x*.r))::[]))


let rec find_tree: quadtree -> coord -> coord option =
fun tree coord_balle ->
	match tree with
		|Leaf (_, None) -> None
		|Leaf (_, brick) -> brick
		|Node (limites, br_ne, br_se, br_no, br_so) ->
			match placement coord_balle limites with
				|true,true   -> find_tree br_ne coord_balle
				|true,false  -> find_tree br_se coord_balle
				|false,true  -> find_tree br_no coord_balle
				|false,false -> find_tree br_so coord_balle

let rec appartient : coord -> coord list -> bool =
fun coord_test list_test ->
	match list_test with
		|[]     -> false
		|(t::q) -> if equal t coord_test then true else appartient coord_test q

let find_briques : quadtree -> (coord * (float * float)) -> coord list =
fun tree balle ->
	let rec aux_briq : quadtree -> coord list -> coord list =
	fun tree coord_pred ->
		match coord_pred with
			|[]     -> []
			|(t::q) ->
				(match find_tree tree t with
					|None     -> aux_briq tree q
					|Some(co) -> co::(aux_briq tree q))
	in
	let rec clean_doublon : coord list -> coord list -> coord list=
	fun list_propre list_sale ->
		match list_sale with
			|[]     -> list_propre
			|(t::q) -> if appartient t list_propre then clean_doublon list_propre q else clean_doublon (t::list_propre) q
	in
		clean_doublon [] (aux_briq tree (predict BalleInit.radius balle ))

let insert_tree : quadtree -> coord -> quadtree =
fun tree coord_new_br ->
	let rec aux : quadtree -> coord -> quadtree =
	fun tree coord_new_br ->
		match tree with
			|Leaf (lim, None)  -> Leaf (lim, Some(coord_new_br))
			|Leaf (lim, Some(coord_old_br)) ->
				if equal coord_old_br coord_new_br
					then Leaf(lim, Some(coord_old_br))
					else
						let new_node = Node(lim, Leaf(quadrant_ne lim, None), Leaf(quadrant_se lim, None), Leaf(quadrant_no lim, None), Leaf(quadrant_so lim, None)) in
						aux (aux new_node coord_old_br) coord_new_br
			|Node (lim, br_ne, br_se, br_no, br_so) ->
				(match placement coord_new_br lim with
					|true,true   -> Node (lim, (aux br_ne coord_new_br), br_se, br_no, br_so)
					|true,false  -> Node (lim, br_ne, (aux br_se coord_new_br), br_no, br_so)
					|false,true  -> Node (lim, br_ne, br_se, (aux br_no coord_new_br), br_so)
					|false,false -> Node (lim, br_ne, br_se, br_no,(aux br_so coord_new_br)) )
	in
		match find_tree tree coord_new_br with
			|None    -> aux tree coord_new_br
			|Some(coord) -> if (equal coord coord_new_br) then tree else aux tree coord_new_br

let create_tree : limites -> coord list -> quadtree =
fun lim briques ->
	let rec aux : quadtree -> coord list -> quadtree =
	fun tree briques -> match briques with
		|[] -> tree
		|(t::q) -> aux (insert_tree tree t) q
		in aux (Leaf(lim, None)) briques


(******************************************************************************)
(*      fonction interne de nettoyage du quadtree pour eviter les             *)
(*                       embranchement inutiles.                              *)
(*   signature :                                                              *)
(*      clean : quadtree -> quadtree                                          *)
(*   paramètre(s) : le quadtree à purifier                                    *)
(*   résultat     : le quadtree post opération, plus propre.                  *)
(******************************************************************************)
let rec clean : quadtree -> quadtree =
fun tree ->
	let clean_etage : quadtree -> quadtree =
	fun tree -> match tree with
		|Node (lim, Leaf(_, None), Leaf(_, None), Leaf(_, None), Leaf(_, None)) -> Leaf(lim, None)
		|Node (lim, Leaf(_, Some(coord)), Leaf(_, None), Leaf(_, None), Leaf(_, None))  -> Leaf(lim, Some(coord))
		|Node (lim, Leaf(_, None), Leaf(_, Some(coord)), Leaf(_, None), Leaf(_, None))  -> Leaf(lim, Some(coord))
		|Node (lim, Leaf(_, None), Leaf(_, None), Leaf(_, Some(coord)), Leaf(_, None)) -> Leaf(lim, Some(coord))
		|Node (lim, Leaf(_, None), Leaf(_, None), Leaf(_, None), Leaf(_, Some(coord)))  -> Leaf(lim, Some(coord))
		|_ -> tree
		in
	match tree with
		|Leaf (_, _) -> tree
		|Node (lim, br_ne, br_se, br_no, br_so) -> Node(lim, clean (clean_etage br_ne), clean (clean_etage br_se), clean (clean_etage br_no), clean (clean_etage br_so))

(******************************************************************************)
(*      fonction interne qui retire une brique d'un quadtree.                 *)
(*   signature :                                                              *)
(*      kill : quadtree -> coord -> quadtree                                  *)
(*   paramètre(s) : le quadtree d'où l'on veut retirer la brique.             *)
(*                  les coordonnées de la brique à retirer.                   *)
(*   résultat     : le quadtree post opération, plus propre.                  *)
(******************************************************************************)
let rec kill : quadtree -> coord -> quadtree =
fun tree coord_cible ->
	match tree with
		|Leaf(_, None) -> tree
		|Leaf(lim, Some(coord)) -> if (equal coord coord_cible) then let (x,y) = coord in Leaf(lim, None) else tree
		|Node (lim, br_ne, br_se, br_no, br_so) ->
			match placement coord_cible lim with
				|true,true   -> Node (lim, (kill br_ne coord_cible), br_se, br_no, br_so)
				|true,false  -> Node (lim, br_ne, (kill br_se coord_cible), br_no, br_so)
				|false,true  -> Node (lim, br_ne, br_se, (kill br_no coord_cible), br_so)
				|false,false -> Node (lim, br_ne, br_se, br_no,(kill br_so coord_cible))

let rec purge_tree : quadtree -> coord list -> quadtree =
fun tree briques ->
	match briques with
		|[]     -> clean tree
		|(t::q) -> purge_tree (kill tree t) q

let rec draw_briques : quadtree -> int -> int -> unit =
fun tree width height ->
	let rec deg_briques x y w h i = if (w>1) && (h>1) then
	(	(Graphics.set_color (Graphics.rgb 0 0 i)) ; Graphics.fill_rect x y w h; deg_briques (x+1) (y+1) (w-2) (h-2) (i-25)) else () in

	match tree with
		| Leaf (_,co) ->
			( match co with
				| None -> ()
				| Some (x1,y1) ->
									 deg_briques (int_of_float x1) (int_of_float y1) width height 255;
			)
		| Node (_,t1,t2,t3,t4) -> draw_briques t1 width height;
			 draw_briques t2 width height;
			 draw_briques t3 width height;
			 draw_briques t4 width height

