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

  let quadtreeB = create_tree (Box.supx, Box.supy/2) liste_brique in

  (balle, raquette, score, (quadtreeB, List.length liste_brique))



let raquette_update = 
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let dx = (x -. prev_x) /. Init.dt in
      Some ((float_of_int x, dx, Graphics.button_down ()), x))
    ()

let balle_update : raquette Flux.t -> raquette -> balle -> quadtree -> balle Flux.t =
  fun raquette_flux
    (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtreeB -> 

    let collision (x,y) (dx,dy) = 
      let is_brique = find_brique quadtreeB (x, y) in

      match is_brique with 
      | None -> false
      | Some (x, y) -> if is_colliding ((x,y), BalleInit.radius) ((x,y), (TailleBriqueInit.width, TailleBriqueInit.height)) (dx, dy)  = (0.0,0.0) then false else true
    in


    let rec run : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->
        let g = 9.81 in
        let a = Flux.constant (0.0, -.g) in
        let v = Flux.map (fun (vx, vy) -> (vx +. dx, vy +. dy)) (integre F.dt a) in
        let p = Flux.map (fun (px, py) -> (px +. x, py +. y)) (integre F.dt v) in
        Flux.unless (Flux.map2 (fun pn vn -> (pn, vn)) p v) (fun ((x,y),(dx,dy)) -> collision (x,y) (dx,dy)) (fun ((x,y), (dx, dy)) -> run_collision ((x, y), (rebond_x x dx, rebond_y y dy)))
      in


    let run_collision : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->

        let g = 9.81 in
        let a = Flux.constant (0.0, -.g) in

        match (is_colliding ((x,y), BalleInit.radius) ((x,y), (TailleBriqueInit.width, TailleBriqueInit.height)) (dx, dy)) with
          | (0.0,0.0) -> run_out_quadtree ((x,y), (dx, dy))
          | (-1.0,0.0) -> run_out_quadtree ((x,y), (rebond_x x dx, dy))
          | (1.0,0.0) -> run_out_quadtree ((x,y), (rebond_x x dx, dy))
          | (0.0,-1.0) -> run_out_quadtree ((x,y), (dx, rebond_y y dy))
          | (0.0,1.0) -> run_out_quadtree ((x,y), (dx, rebond_y y dy))
      in


    let run_out_quadtree : etat -> etat Flux.t = 
      fun ((x0,y0),(dx0,dy0)) -> 
        let a = Flux.constant (0., -9.81) in
  
        let v = Flux.map (fun (a , b) -> (a +. dx0, b +. dy0)) (integre F.dt a) in
  
        let p = Flux.map (fun (a , b) -> (a +. x0, b +. y0)) (integre F.dt v) in
        Flux.map2 (fun pn vn -> (pn, vn)) p v  
      in



    if y > Box.supy then 
      run ((x, y), (dx, dy))
    else
      if y > RaquetteInit.ypos then
        run_out_quadtree ((x, y), (dx, dy))
        
      else
        if (y - RaquetteInit.ypos) < BalleInit.radius + RaquetteInit.height then
          run ((x, y), (dx, -.dy))
        else
          run_out_quadtree ((x, y), (dx, dy))
          
        



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

    let (x,y), (dx,dy) = balle in*)
    
    

    
  

    
    
          

      
          


      










    






