(* Librairie d'inputs *)
open Iterator
open Init

type vector = float*float
type etat_balle  = vector*vector
type rect = (vector) * (vector)
type ball = (vector) * float

let dot (x1,y1) (x2,y2) : float =
  ((x1 *. x2) +. (y1 *.y2))

let sub (x1,y1) (x2,y2) : vector =
  ((x1 -. x2),(y1 -.y2))

let scale (x,y) s : vector =
  (x *. s, y*.s)

let invert v : vector =
  (scale v (-1.0))

let div (x,y) s : vector =
  (x /. s, y/.s)

let norm (x,y) : float =
  (x**2.0 +. y**2.0)

let proj v p : vector =
  (scale p ((dot v p) /. (norm p)**2.0))

let mirror v m : vector =
  (sub (scale (proj v m) 2.0) v)

let to_string (x,y) : string =
  "("^(Float.to_string x)^","^(Float.to_string y)^")"
(* À LA MÉMOIRE DE LA SOURIS *)
(* let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    () *)


let is_colliding balle rectangle vitesse : vector  =

  let normalise x y : vector =
    let abs = x**2.0 +. y**2.0 in
    (x/.abs),(y/.abs)
  and vect_to_dir (vx,vy) : vector =
    if (vx >= vy) then
      if (vx >= 0.0)
        then (1.0,0.0)
        else (-1.0,0.0)
    else
      if (vy >= 0.0)
        then (0.0,1.0)
        else (0.0,-1.0)
  in
  let is_point_in_ball_n px py cx cy r : vector =
    if (((px -. cx)**2.0) +. (( py -. cy)**2.0 ) <= r**2.0) 
      then (normalise (cx -. px) (cy -. py))
      else (0.0,0.0)
  and is_point_in_rect px py rx ry rtx rty : bool =
    if ((rx <= px) && (px <= (rx +. rtx)) && (ry <= py) && (py <= (ry +. rty))) then true else false
  in

  let get_normal : vector =
    (match balle with
    | (cx,cy),r -> 
      match rectangle with
      | (rx,ry),(rtx,rty) ->
        if (cx < rx) then (* Si à gauche *)
          if (cy < ry) then (* Si à gauche et en dessous *)
            (print_endline("à gauche et en dessous");
            (is_point_in_ball_n (rx) (ry) cx cy r))
          else if (cy > ry +. rty) then (* Si à gauche et au dessus *)
            (print_endline("à gauche et au dessus");
            (is_point_in_ball_n (rx) (ry +. rty) cx cy r))
          else (* Si à gauche et dedans (y) *)
          (print_endline("à gauche et dedans (y)");
            if (is_point_in_rect (cx+.r) cy rx ry rtx rty)
              then (-1.0,0.0)
              else (0.0,0.0)
          )
        else if (cx > rx +. rtx) then (* Si à droite *)
          if (cy < ry) then (* Si à droite et en dessous *)
            (print_endline("à droite et en dessous");
            (is_point_in_ball_n (rx+.rtx) (ry) cx cy r))
          else if (cy > ry +. rty) then (* Si à droite et au dessus *)
            (print_endline("à droite et au dessus");
            (is_point_in_ball_n (rx +. rtx) (ry +. rty) cx cy r))
          else (* Si à droite et dedans (y) *)
          (print_endline("à droite et dedans (y)");
            if (is_point_in_rect (cx+.r) cy rx ry rtx rty)
              then (1.0,0.0)
              else (0.0,0.0)
          )
        else (* Si dedans (x) *)
          if (cy < ry) then (* Si dedans (x) et en dessous *)
            (print_endline("dedans (x) et en dessous");
            (0.0,-1.0))
          else if (cy > ry +. rty) then (* Si dedans (x) et au dessus *)
            (print_endline("dedans (x) et au dessus ");
            (0.0,1.0))
          else (* Si dedans (x) et dedans (y) *)
          (print_endline("dedans (x) et dedans (y)");
              (vect_to_dir vitesse))
    ) in
    match get_normal with
    | (0.0,0.0) -> (0.0,0.0)
    | normal ->
      if ((dot normal vitesse) <= 0.0) then 
      (
        print_endline("Normal is : "^(to_string normal)^", length : "^(Float.to_string (norm normal)));
        print_endline("Old speed is : "^(to_string (vitesse))^", length : "^(Float.to_string (norm vitesse)));
        let new_speed = mirror (invert vitesse) normal in
        (
          print_endline("New speed is : "^(to_string new_speed)^", length : "^(Float.to_string (norm new_speed)));
          new_speed
        )
      ) else
      (
        print_endline("Collision detected but culled.");
        (0.0,0.0)
      )

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

  let res = is_colliding ((x,y), BalleInit.radius) ((bx, by), (float_of_int TailleBriqueInit.width, float_of_int TailleBriqueInit.height)) (dx,dy) in
  
    if (y +. BalleInit.radius -. (float_of_int RaquetteInit.ypos) < 2.) && (x >= (float_of_int mouse_x) && x <= (float_of_int mouse_x) +. (float_of_int RaquetteInit.width)) then
  
      (dx +. mouse_dx,-.dy)

    else 
      match res with
        |(0.0,0.0) -> (rebond_x x dx, rebond_y y dy)
        |(1.0,0.0) |(-1.0,0.0) -> (-.dx,dy)
        |(0.0,1.0) |(0.0,-1.0) -> (dx,-.dy)
        |_ -> failwith "pas possible"
        

    

    











end
            


