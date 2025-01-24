(* Librairie d'inputs *)
open Iterator
open Init


type etat_balle  = (float*float)*(float*float)
type rect = (float*float)*(float*float)
type ball = (float*float)*float




(* À LA MÉMOIRE DE LA SOURIS *)
(* let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    () *)



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
        if (is_point_in_rect (cx) (cy+.r) rx ry rtx rty)
          then (0.0,-1.0)
          else (0.0,0.0)
      else if (cy > ry +. rty) then (* Si dedans (x) et au dessus *)
        if (is_point_in_rect (cx) (cy+.r) rx ry rtx rty)
          then (0.0,1.0)
          else (0.0,0.0)
        else (* Si dedans (x) et dedans (y) *)
            (vect_to_dir (vx,vy))

let integre dt flux =
  let init = (0., 0.) in
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  let rec acc =
    Tick (lazy (Some (init, Flux.map2 iter acc flux)))
  in acc

module Collision = struct
  let dt = Data.dt

  let contact_x x dx = 
    if (x > Box.supx) && (dx > 0.) then true
    else if (x < Box.infx) && (dx < 0.) then true
    else false



  let contact_y y dy = 
    if (y > Box.supy) && (dy > 0.) then true
    else if (y < Box.infy) && (dy < 0.) then true
    else false

    let rebond_x x dx = 
      if contact_x x dx then (-.dx) else dx
  
    let rebond_y y dy = 
      if contact_y y dy then (-.dy) else dy
  


  let rebond (x,y) (dx,dy) blist (mouse_x, mouse_dx) = 

    let bx, by = List.hd blist in
    
    
    
    if (y -. BalleInit.radius < ((float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height))) (*&& (dy < 0.)) && ((x >= (mouse_x)) && x <= (mouse_x +. float_of_int RaquetteInit.width)) *) then
      
      
      (dx +. mouse_dx , -.dy)

    else 
      match is_colliding ((x,y), BalleInit.radius) ((bx, by), (float_of_int TailleBriqueInit.width, float_of_int TailleBriqueInit.height)) (dx,dy) with
        |(0.0,0.0) -> ((rebond_x x dx, rebond_y y dy))
        |(1.0,0.0) |(-1.0,0.0) -> (-.dx,dy)
        |(0.0,1.0) |(0.0,-1.0) -> (dx,-.dy)
        |_ -> failwith "pas possible"
        

    

    











end
            


