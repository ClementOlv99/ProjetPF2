open Quadtree
open Iterator
open Input
open Init

(*etat de la partie*)

type raquette = (float * float)
type balle = (float * float) * (float * float)
type score = (int * int)
type etat = balle * raquette * score * (quadtree * int)

let game_init liste_brique = 

  let balle = ((Box.supx/.10. +. 50., Box.supy -. 130.), (50., 0.)) in 

  let raquette = (0., 0.) in  

  let score = 0, 3 in

  let quadtreeB = create_tree ((0.,0.),(Box.supx, Box.supy)) in

  (balle, raquette, score, (quadtreeB, List.length liste_brique))



let raquette_update = 
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let dx = ((float_of_int x) -. prev_x) /. Data.dt in
      Some ((float_of_int x, dx), prev_x))
      0.0

let balle_update : raquette -> balle -> quadtree -> balle Flux.t =
  fun (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtreeB -> 

    let g = 9.81 in

    let collision (x,y) (dx,dy) = 
      let is_brique = find_briques quadtreeB ((x, y),(dx,dy)) in
      
      

      let rec aux is_brique_aux =
        match is_brique_aux with 
        | [] -> 0
        | briquecoord::q ->  let(a,b) = is_colliding ((x,y), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (dx,dy) in
                            (*print_endline(string_of_float a);print_endline(string_of_float b);*)
                            if (a,b) <> (0.0,0.0) then 1 + aux q else aux q
      in

      aux is_brique
    in



    let rec run : bool -> balle -> balle Flux.t =
      fun pastouche ((x,y), (dx, dy)) ->

        let a = Flux.constant (0.0, -.g) in
        let v = Flux.map (fun (vx, vy) -> (vx +. dx, vy +. dy)) (integre Data.dt a) in
        let p = Flux.map (fun (px, py) -> (px +. x, py +. y)) (integre Data.dt v) in
        print_endline (string_of_bool pastouche);
        Flux.unless 
              (Flux.unless 
                    (Flux.map2 (fun pn vn -> (pn, vn)) p v) (fun ((x,y),(dx,dy)) -> ((collision (x,y) (dx,dy) = 1) && pastouche)|| Collision.contact_x x dx || Collision.contact_y y dy || ((y -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (dy < 0.))) 
                          (fun ((x,y), (dx, dy)) -> run false ((x, y), (Collision.rebond (x,y) (dx,dy) (find_briques quadtreeB ((x,y),(dx,dy))) (mouse_x, mouse_dx))))) (fun ((x,y),(dx,dy)) -> collision (x,y) (dx,dy) = 2) (fun ((x,y),(dx,dy)) -> run true ((x,y), (-.dx, -.dy)))

      in

    run true ((x,y), (dx, dy))

    
          
        



let score_update : score -> balle -> (score Flux.t * int) = 
  fun (current_score, lives) ((x, y), (_, _)) ->
    if y < 0. then
      if (lives - 1) = 0 then
        (Flux.constant (current_score, lives - 1), 0)
      else
        (Flux.constant (current_score, lives - 1), 1)

    else
        (Flux.constant (current_score, lives), 1)


let game_update : etat -> etat Flux.t =
  fun (balle, raquette, score, (quadtreeB, nbBrique)) ->
    
    let balle_flux = balle_update  raquette balle quadtreeB in
    
    let (x,y), (dx,dy) = balle in 

    (*let briques = find_briques quadtreeB (x,y) (dx,dy) in*)
    let nbBrique = 16 in
    (*to do: 
     - Détruire brique
    *) 

    let raquette_flux = raquette_update in

    let (score_flux, lives) = score_update score balle in


    if nbBrique = 0 || lives = 0  then
      Flux.constant (balle, raquette, score, (quadtreeB, nbBrique))
    else
      let map4 f i1 i2 i3 i4 = Flux.(apply (apply (apply (apply (constant f) i1) i2) i3) i4) in

      map4 (fun b r s q -> (b, r, s, q)) balle_flux raquette_flux score_flux (Flux.constant (quadtreeB, nbBrique))

    

      


    







    






