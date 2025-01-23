open Quadtree
open Iterator
open Input
open Init
open Dessin

(*etat de la partie*)

type raquette = (float * float)
type balle = (float * float) * (float * float)
type score = (int * int)
type etat = balle * raquette * score * (quadtree * int)

let game_init liste_brique = 

  let balle = ((Box.supx/.2., Box.supy/.2.), (0., 0.)) in

  let raquette = mouse in

  let score = 0, 3 in

  let quadtreeB = create_tree ((0.,0.),(Box.supx, Box.supy)) liste_brique in

  (balle, raquette, score, (quadtreeB, List.length liste_brique))



let raquette_update = 
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let dx = ((float_of_int x) -. prev_x) /. Data.dt in
      Some ((float_of_int x, dx, Graphics.button_down ()), float_of_int x))
      0.0

let balle_update : raquette Flux.t -> raquette -> balle -> quadtree -> balle Flux.t =
  fun raquette_flux
    (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtreeB -> 

    let g = 9.81 in

    let collision (x,y) (dx,dy) = 
      (*let is_brique = find_briques quadtreeB (x, y) (dx,dy) in*)

      let is_brique = [] in

      let rec aux is_brique_aux =
        match is_brique with 
        | [] -> 0
        | briquecoord::q -> if is_colliding ((x,y), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (dx,dy) = (0.0,0.0) then 1 + aux q else aux q
      in

      aux is_brique
    in


    let rec run : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->
        
        let a = Flux.constant (0.0, -.g) in
        let v = Flux.map (fun (vx, vy) -> (vx +. dx, vy +. dy)) (Collision.integre Data.dt a) in
        let p = Flux.map (fun (px, py) -> (px +. x, py +. y)) (Collision.integre Data.dt v) in
        Flux.unless (Flux.map2 (fun pn vn -> (pn, vn)) p v) (fun ((x,y),(dx,dy)) -> collision (x,y) (dx,dy) || contact_x x dx || contact_y y dy) (fun ((x,y), (dx, dy)) -> run_collision ((x, y), (rebond_x x dx, rebond_y y dy)))
      in


    let run_collision : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->

        let a = Flux.constant (0.0, -.g) in

        match (is_colliding ((x,y), BalleInit.radius) ((x,y), (TailleBriqueInit.width, TailleBriqueInit.height)) (dx, dy)) with
          | (-1.0,0.0) | (1.0,0.0) -> run ((x,y), (-.dx, dy))
          | (0.0,-1.0) | (0.0,1.0) -> run ((x,y), (dx, -.dy))

          | (0.0,0.0) -> if contact_x x dx then run ((x,y), (-.dx, dy)) else  if contact_y y dy then run ((x,y), (dx, -.dy)) else run ((x,y), (dx, dy))
      in

      run ((x,y), (dx, dy))

    
          
        



let score_update : score -> int -> balle -> score Flux.t = 
  fun (current_score, lives) ((x, y), (_, _)) ->
    if y < 0. then
      Flux.constant (current_score, lives - 1)
    else
      Flux.constant (current_score, lives)


(*let game_update : etat -> etat Flux.t =
  fun (balle, raquette, score, (quadtreeB, nbBrique)) ->
    let raquette_flux = raquette_update in
    let balle_flux = balle_update raquette_flux raquette balle quadtreeB in
    let score_flux = score_update score nbBrique balle in

    let (x,y), (dx,dy) = balle in
*)
    







    






