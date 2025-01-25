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

  let balle = ((Box.supx/.10. +. 200., Box.supy -. 100.), (30., -50.)) in 

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



    let rec run : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->

        let a = Flux.constant (0.0, -.g) in
        let v = Flux.map (fun (vx, vy) -> (vx +. dx, vy +. dy)) (integre Data.dt a) in
        let p = Flux.map (fun (px, py) -> (px +. x, py +. y)) (integre Data.dt v) in
        

        Flux.unless 
          (Flux.map2 (fun pn vn -> (pn,vn)) p v) 
            (fun ((x,y), (dx,dy)) -> ((collision (x,y) (dx,dy) <> 0))|| Collision.contact_x x dx || Collision.contact_y y dy || ((y -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (dy < 0.) && (x >= float_of_int (fst (Graphics.mouse_pos ())) && x <= ((float_of_int (fst (Graphics.mouse_pos ()))) +. (float_of_int RaquetteInit.width))))) 
              (fun ((x,y), (dx, dy)) -> run (Collision.rebond (x,y) (dx,dy) (find_briques quadtreeB ((x,y),(dx,dy))) (float_of_int (fst (Graphics.mouse_pos ())), mouse_dx)))
      in

    run  ((x,y), (dx, dy))

    
    let quadtree_update : quadtree -> balle -> (quadtree * int) Flux.t =
      fun quadtree ((x, y), (dx, dy)) ->
        print_endline(string_of_float x);
        let a_supprimer quadtree ((x, y), (dx, dy))=

        let briques = find_briques quadtree ((x, y), (dx, dy)) in

          let rec aux briques l_col =
            match briques with
            | [] -> l_col
            | briquecoord::q ->
                let (a, b) = is_colliding ((x, y), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (dx, dy) in
                if (a, b) <> (0.0, 0.0) then
                  aux q (briquecoord::l_col)
                else
                  aux q l_col
          in

          let colliding_briques = aux briques [] in

          if List.length colliding_briques = 0 then
            (quadtree, List.length colliding_briques)
          else
            (purge_tree quadtree colliding_briques, List.length colliding_briques)
          
        in
        
      Flux.map
        (fun quadtree -> (a_supprimer quadtree ((x, y), (dx, dy))))
          (Flux.constant quadtree)         
        



let score_update : score -> balle -> (score Flux.t * int) = 
  fun (current_score, lives) ((x, y), (_, _)) ->
    if y < 0. then
      if (lives - 1) = 0 then
        (Flux.constant (current_score, lives - 1), 0)
      else
        (Flux.constant (current_score, lives - 1), 1)

    else
        (Flux.constant (current_score, lives), 1)


let rec game_update : etat -> etat Flux.t =
  fun (balle, raquette, score, (quadtreeB, nbBrique)) ->
    
    let balle_flux = balle_update  raquette balle quadtreeB in

    let nbBrique = 16 in


    let raquette_flux = raquette_update in

    let (score_flux, lives) = score_update score balle in

    let quadtreeB_flux = quadtree_update quadtreeB balle in

    let cond b q s r = 
      
      let (score, lives) = s in
      
      let (nx, ny), (ndx, ndy) = b in

      let quadtree, _ = q in

      let briques = find_briques quadtree ((nx, ny), (ndx, ndy)) in
      ((print_endline(string_of_float nx));(print_endline(string_of_float ny)));

      let rec aux_cond briques balle =
        match briques with
        | [] -> print_endline("cringe");false 
        | briquecoord::q ->
            let (a, b) = is_colliding ((fst balle), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (snd balle) in
            if (a, b) <> (0.0, 0.0) then true else aux_cond q balle
      in

      aux_cond briques ((nx, ny), (ndx, ndy))
    
    in

  


    let map4 f i1 i2 i3 i4 = Flux.(apply (apply (apply (apply (constant f) i1) i2) i3) i4) in

    let flux_normal = map4 (fun b r s q -> (b, r, s, q)) balle_flux raquette_flux score_flux quadtreeB_flux in

    Flux.unless flux_normal (fun (b, r, s, q) -> cond b q s r) (fun (b, r, s, q) -> game_update (b, r, s, q))




    

      


    







    






