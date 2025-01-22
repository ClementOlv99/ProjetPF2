open Init


(*                          Module QuadTree                                   *)

type tree
type coord
type limites

(******************************************************************************)
(*                                                                            *)
(*      fonction de calcul des points de la prédiction de la balle            *)
(*                                                                            *)
(*   signature :                                                              *)
(*		predict : int -> (int*int) -> (float * float) -> coord list  	 	  *)
(*                                                                            *)
(*   paramètre(s) : le rayon de la balle									  *)
(*					les coordonnées de la balle								  *)
(*					les vitesses de la balle								  *)
(*   résultat     : les trois points de test de colisions future		      *)
(*                                                                            *)
(******************************************************************************)
val predict : int -> balle -> coord list


(******************************************************************************)
(*                                                                            *)
(*      fonction de création du quadtree initial				              *)
(*                                                                            *)
(*   signature : create_tree : (int * int) list -> quadtree                   *)
(*                                                                            *)
(*   paramètre(s) : les limites de l'écran									  *)
(*					la liste des briques                                      *)
(*   résultat     : un quadtree avce la position de chacunes des briques      *)
(*                                                                            *)
(******************************************************************************)
val create_tree : limites -> coord list -> tree

(******************************************************************************)
(*                                                                            *)
(*      fonction de recherche de brique à portée 					 	      *)
(*                                                                            *)
(*   signature : find_brique : quadtree -> coord -> coord option  *)
(*                                                                            *)
(*   paramètre(s) : le quadtree representant le niveau						  *)
(*					un couple de float representant la position de la balle   *)
(*   résultat     : None si pas de briques à portée							  *)
(*  	Some (coord) les coordonnées de la brique incriminée sinon	      *)
(*                                                                            *)
(******************************************************************************)
val find_brique : tree -> coord -> coord option

(******************************************************************************)
(*                                                                            *)
(*      fonction de retrait d'une brique d'un quadtree                        *)
(*                                                                            *)
(*   signature : destroy : quadtree -> (int * int) -> quadtree                *)
(*                                                                            *)
(*   paramètre(s) : le quadtree representant le niveau						  *)
(*					un couple d'entiers representant la position de la brique *)
(*   résultat     : le quadtree corrigé                                       *)
(*                                                                            *)
(******************************************************************************)
val destroy : tree -> coord -> tree

