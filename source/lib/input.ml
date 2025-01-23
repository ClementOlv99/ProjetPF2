open Iterator
open Init

(* flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()

type etat_balle = (float*float)*(float*float)
type rect = (float*float)*(float*float)
type ball = (float*float)*float

let is_colliding balle rectangle (vx,vy) : (float*float)  =

  let normalise x y : (float*float) =
    let abs = x**2.0 +. y**2.0 in
    (x/.abs),(y/.abs)
  and vect_to_dir (vx,vy) : (float*float) =
    if (vx >= vy) then
      if (vx >= 0.0)
        then (1.0,0.0)
        else (-1.0,0.0)
    else
      if (vy >= 0.0)
        then (0.0,1.0)
        else (0.0,-1.0)
  in
  let is_point_in_ball_n px py cx cy r : (float*float) =
    if (((px -. cx)**2.0) +. (( py -. cy)**2.0 ) <= r**2.0) 
      then (normalise (cx -. px) (cy -. py))
      else (0.0,0.0)
  and is_point_in_rect px py rx ry rtx rty : bool =
    if ((rx <= px) && (px <= (rx +. rtx)) && (ry <= py) && (py <= (ry +. rty))) then true else false
  in

  match balle with
  | (cx,cy),r -> 
    match rectangle with
    | (rx,ry),(rtx,rty) ->
      if (cx < rx) then (* Si à gauche *)
        if (cy < ry) then (* Si à gauche et en dessous *)
          (is_point_in_ball_n (rx) (ry) cx cy r)
        else if (cy > ry +. rty) then (* Si à gauche et au dessus *)
          (is_point_in_ball_n (rx) (ry +. rty) cx cy r)
        else (* Si à gauche et dedans (y) *)
          if (is_point_in_rect (cx+.r) cy rx ry rtx rty)
            then (-1.0,0.0)
            else (0.0,0.0)
      else if (cx > rx +. rtx) then (* Si à droite *)
        if (cy < ry) then (* Si à droite et en dessous *)
          (is_point_in_ball_n (rx+.rtx) (ry) cx cy r)
        else if (cy > ry +. rty) then (* Si à droite et au dessus *)
          (is_point_in_ball_n (rx +. rtx) (ry +. rty) cx cy r)
        else (* Si à droite et dedans (y) *)
          if (is_point_in_rect (cx+.r) cy rx ry rtx rty)
            then (1.0,0.0)
            else (0.0,0.0)
      else (* Si dedans (x) *)
        if (cy < ry) then (* Si dedans (x) et en dessous *)
          (0.0,-1.0)
        else if (cy > ry +. rty) then (* Si dedans (x) et au dessus *)
          (0.0,1.0)
        else (* Si dedans (x) et dedans (y) *)
            (vect_to_dir (vx,vy))

module Collision = struct

  let dt = Data.dt

  let integre dt flux =
    (* valeur initiale de l'intégrateur                         *)
    let init = ( 0., 0.) in
    (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
    let iter (acc1, acc2) (flux1, flux2) =
      (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
    (* définition récursive du flux acc                         *)
    let rec acc =
      Tick (lazy (Some (init, Flux.map2 iter acc flux)))
    in acc;;

  let rec unless flux cond f_flux =
    match Flux.uncons flux with
    | None -> Flux.vide
    | Some (a, fl) -> if cond a then f_flux a else Flux.cons a (unless fl cond f_flux)
    
    
end
