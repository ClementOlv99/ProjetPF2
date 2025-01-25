(*                          Module QuadTree                                   *)

type quadtree
(**
	le type du QuadTree à proprement parler.
*)

type coord = (float * float)
(**
	pour stocker les coordonnées de différents objets: coord = (float * float) .
*)
type limites =coord * coord
(**
	pour delimiter un espace, on stock le coin inferieur gauche (noté dans le code "sud-est") et le coin superieur droit (noté dans le code "nord-ouest"): limites = (coord * coord) .
*)

val predict : float -> ((float * float) * (float * float)) -> coord list
(**
	fonction de calcul des points de la prédiction de la balle.
	- signature : predict : float -> ((float * float) * (float * float)) -> coord list

	- paramètre(s) :
		- le rayon de la balle.
		- (les coordonnées de la balle * les vitesses de la balle).

	- résultat :
		- les trois points de test de colisions future.
*)

val create_tree : limites -> quadtree
(**
	fonction de création du QuadTree initial.
	- signature : create_tree : coord list -> tree

	- paramètre(s) :
		- les limites de l'écran.
		- la liste des briques.

	- résultat :
		- un QuadTree avec la position de chacunes des briques.
*)

val insert_tree : quadtree -> coord -> quadtree
(**
	fonction pour rajouter une brique dans un QuadTree en respectant la structure du QuadTree et ses invariants.
	- insert_tree : tree -> coord -> tree

	- paramètre(s) :
		- le QuadTree incomplet.
		- les coordonnées de la brique à rajouter.

	- résultat :
		- le QuadTree avec la brique en plus.
*)

val find_tree : quadtree -> coord -> coord option
(**
	fonction de recherche de brique à portée.

	- signature : find_tree : tree -> coord -> coord option

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- un couple de float representant la position de la balle.

	- résultat :
		- None si pas de briques à portée.
		- Some (coord) les coordonnées de la brique incriminée sinon.
*)

val find_briques : quadtree -> (coord * (float * float)) -> coord list
(**
	fonction de recherche des briques à portées d'une balle (à priori les briques les plus proches des trois points de la projection de la balle.)

	- find_briques : quadtree -> coord list -> coord list

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- un coord representant la position de la balle et un couple de float representant la vitesse de la balle.

	- résultat :
		- Une liste des briques potentiellement à portées.
*)

val purge_tree : quadtree -> coord list -> quadtree
(**
	fonction de retrait d'une liste de briques d'un QuadTree.

	- signature : purge_tree : quadtree -> coord -> quadtree

	- paramètre(s) :
		- le QuadTree representant le niveau.
		- une liste de couple d'entiers representant les positions des briques à retirer.

	- résultat :
		- le QuadTree corrigé.
*)

val draw_briques : quadtree -> int -> int -> unit
