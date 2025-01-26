open Init


let draw_raquette (xpos,_) = Graphics.draw_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height;
                         Graphics.set_color Graphics.blue;
                         Graphics.fill_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height
(**
	fonction de dessin de la raquette
	- signature : draw_raquette : (float * float)-> unit

	- paramètre(s) :
		- la position en x de la raquette

	- résultat :
		- la raquette est dessiné sur le graphique
*)

let draw_balle b = 
  match b with
    | ((x,y),(dx,dy)) -> Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius);
                   Graphics.set_color Graphics.red;
                   Graphics.fill_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius)
    | _ -> ()
(**
	fonction de dessin de la balle
	- signature : draw_balle : ((float * float) * (float * float)) ->  unit

	- paramètre(s) :
		- les coordonnées et vitesses de la balle

	- résultat :
		- la balle est déssinée
*)

let draw_life l =
  Graphics.moveto 10 22;
  Graphics.set_color Graphics.green;
  Graphics.draw_string ("nombre de vie : " ^ (string_of_int l))
(**
	fonction de dessin des points de vie du joueur
	- signature : draw_life : float -> unit

	- paramètre(s) :
		- le nombre de vie

	- résultat :
		- le dessin du nombre de point de vie
*)

let draw_score s =
  Graphics.moveto 10 35;
  Graphics.draw_string ("score : " ^ (string_of_int s))
(**
	fonction qui dessine le score
	- signature : draw_score : float -> unit

	- paramètre(s) :
		- le score du joueur

	- résultat :
		- le dessin du nombre de point du joueur
*)


let draw_nbrique n = 
  (Graphics.moveto 10 10;
  (Graphics.draw_string ("nombre de briques : " ^(string_of_int n))))
(**
	fonction de dessin du nombre de brique
	- signature : draw_nbrique : float -> unit

	- paramètre(s) :
		- nombre de briques

	- résultat :
		- le dessin du nombre de briques
*)
