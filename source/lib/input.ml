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
  Float.sqrt (x**2.0 +. y**2.0)

let proj v p : vector =
  (scale p ((dot v p) /. (norm p)**2.0))

let mirror v m : vector =
  (sub (scale (proj v m) 2.0) v)

let normalise (x,y) : vector =
  let abs = norm (x,y) in
  (x/.abs,y/.abs)

let rotated (x,y) a : vector =
  let angle : float = a *. (Float.pi /. 180.0) in
  ((Float.cos angle)*.x -. (Float.sin angle)*.y,
  (Float.sin angle)*.x +. (Float.cos angle)*.y)

let to_string (x,y) : string =
  "("^(Float.to_string x)^","^(Float.to_string y)^")"

let clamp x a b : float =
  (Float.max a (Float.min b x))

(* À LA MÉMOIRE DE LA SOURIS *)
(* let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    () *)


let is_colliding balle rectangle (vx,vy) : (float*float)  =

    
    let vect_to_dir (vx,vy) : (float*float) =
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
        then
          let d : vector = ((cx -. px),(cy -. py)) in
          (normalise d)
        else (0.0,0.0)
    and is_point_in_rect px py rx ry rtx rty : bool =
      ((rx <= px) && (px <= (rx +. rtx)) && (ry <= py) && (py <= (ry +. rty)))
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
            if (is_point_in_rect (cx-.r) cy rx ry rtx rty)
              then (1.0,0.0)
              else (0.0,0.0)
        else (* Si dedans (x) *)
        if (cy < ry) then (* Si dedans (x) et en dessous *)
          if (is_point_in_rect (cx) (cy+.r) rx ry rtx rty)
            then (0.0,-1.0)
            else (0.0,0.0)
        else if (cy > ry +. rty) then (* Si dedans (x) et au dessus *)
          if (is_point_in_rect (cx) (cy-.r) rx ry rtx rty)
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
    
    if (y -. BalleInit.radius < ((float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height))) 
      && (dy < 0.) 
      && ((x >= (mouse_x)) 
      && (x <= (mouse_x) +. float_of_int RaquetteInit.width))
    then (
      print_endline("Boing !"); 
      let (rx,ry) : vector = (dx +. mouse_dx *. Data.rqtFrictionCoeff, -.dy) in
      print_endline("Speed was : "^(Float.to_string mouse_dx));
      print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
      print_endline("new speed is : "^(to_string (rx,ry))^", de norme "^(Float.to_string (norm (rx,ry))));
      ((x,y +. 1.0),(rx,ry))

    ) else 
      let rec aux2 (x,y) (dx,dy) l = 
        match l with
        | [] -> ((x,y),(rebond_x x dx, rebond_y y dy))
        | (bx,by)::q -> match is_colliding ((x,y), BalleInit.radius) ((bx, by), (float_of_int TailleBriqueInit.width, float_of_int TailleBriqueInit.height)) (dx,dy) with
                        | (0.0,0.0) -> aux2 (x,y) (rebond_x x dx, rebond_y y dy) q
                        | (1.0, 0.0) ->
                          let (vx, vy) : vector = (-.dx, dy) in
                          print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
                          print_endline("new speed is : "^(to_string (vx, vy))^", de norme "^(Float.to_string (norm (vx, vy))));
                          aux2 (x +. vx *. dt *. 2.0,y +. vy *. dt *. 2.0)  (vx, vy) q
                        | (-1.0, 0.0) ->
                          let (vx, vy) : vector = (-.dx, dy) in
                          print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
                          print_endline("new speed is : "^(to_string (vx, vy))^", de norme "^(Float.to_string (norm (vx, vy))));
                          aux2 (x +. vx *. dt *. 2.0,y +. vy *. dt *. 2.0)  (vx, vy) q
                        | (0.0, 1.0) ->
                          let (vx, vy) : vector = (dx, -.dy) in
                          print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
                          print_endline("new speed is : "^(to_string (vx, vy))^", de norme "^(Float.to_string (norm (vx, vy))));
                          aux2 (x +. vx *. dt *. 2.0,y +. vy *. dt *. 2.0)  (vx, vy) q
                        | (0.0, -1.0) ->
                          let (vx, vy) : vector = (dx, -.dy) in
                          print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
                          print_endline("new speed is : "^(to_string (vx, vy))^", de norme "^(Float.to_string (norm (vx, vy))));
                          aux2 (x +. vx *. dt *. 2.0,y +. vy *. dt *. 2.0)  (vx, vy) q
                        | (a,b) ->
                          (* print_endline("normal is : "^(to_string (a,b))^", de norme "^(Float.to_string (norm (a,b))));  *)
                          let (vx, vy) : vector = invert (mirror (dx,dy) (a,b)) in
                          print_endline("old speed is : "^(to_string (dx, dy))^", de norme "^(Float.to_string (norm (dx, dy)))); 
                          print_endline("new speed is : "^(to_string (vx, vy))^", de norme "^(Float.to_string (norm (vx, vy)))); 
                          aux2 (x +. vx *. dt *. 2.0,y +. vy *. dt *. 2.0) (vx, vy) q
                          

      in

      aux2 (x,y) (dx,dy) blist

      
        

    

    











end
            


