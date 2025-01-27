(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game
open Init
open Dessin
open Quadtree

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let draw_state etat = 
  match etat with
    | (b,r,(s,l),(quadtree,n)) -> draw_briques quadtree TailleBriqueInit.width TailleBriqueInit.height;
                              draw_raquette r;
                              draw_balle b;
                              draw_life l;
                              draw_nbrique n

(* extrait le score courant d'un etat : *)
let score etat : int = match etat with (_,_,(s,_),_) -> s

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      draw_score last_score;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Data.dt;
      loop flux_etat' (score etat)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

(*let a = game_init [(12,14); (14,177)]*)

let baseetat = ( ((80.,80.),5.), 40.,0,((create_tree ((0.,0.),(400.,400.))),2))

let () = draw (game_update (game_init))
