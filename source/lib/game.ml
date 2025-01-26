open Quadtree
open Iterator
open Input
open Init

(*etat de la partie*)

type raquette = (float * float)
type balle = (float * float) * (float * float)
type score = (int * int)
type etat = balle * raquette * score * (quadtree * int)

let rec create_level : coord -> coord list =
fun cursor -> match cursor with
  |(_, y) when (y > Box.supy -. Float.of_int TailleBriqueInit.height) -> []
  |(x, y) when (x > Box.supx -. Float.of_int TailleBriqueInit.width) -> create_level(Box.infx, y +. (Float.of_int TailleBriqueInit.height))
  |(x, y)                     -> (x, y)::(create_level (x +. (Float.of_int TailleBriqueInit.width), y))

let liste_brique_num i =
  match i with
  |1 -> create_level (Box.infx, (Box.supy +. Box.infy) /.2.0)
  |2 -> [(100.,100.);(100.,200.);(100.,300.);(100.,400.);(100.,500.);(100.,600.);(200.,100.);(200.,200.);(200.,300.);(200.,400.);(200.,500.);(200.,600.);(300.,100.);(300.,200.);(300.,300.);(300.,400.);(300.,500.);(300.,600.);(400.,100.);(400.,200.);(400.,300.);(400.,400.);(400.,500.);(400.,600.);(500.,100.);(500.,200.);(500.,300.);(500.,400.);(500.,500.);(500.,600.);(600.,100.);(600.,200.);(600.,300.);(600.,400.);(600.,500.);(600.,600.);(700.,100.);(700.,200.);(700.,300.);(700.,400.);(700.,500.);(700.,600.);]
  |_ -> []


let game_init =

  let liste_brique = liste_brique_num 3 in

  let balle = ((Box.supx/.10. +. 200., (Float.of_int RaquetteInit.ypos) +. Box.supy /. 29.), (35., 250.)) in

  let raquette = (0., 0.) in

  let score = 0, 3 in

  let quadtreeB = create_tree ((0.,0.),(Box.supx, Box.supy)) liste_brique in

  (balle, raquette, score, (quadtreeB, List.length liste_brique))





let balle_update : raquette -> balle -> quadtree -> balle Flux.t =
  fun (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtreeB -> 

    let g = Data.gravity in


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
    


    let rec run : balle -> balle Flux.t =
      fun ((x,y), (dx, dy)) ->

        let a = Flux.constant (0.0, -.g) in
        let v = Flux.map (fun (vx, vy) -> (vx +. dx, vy +. dy)) (integre Data.dt a) in
        let p = Flux.map (fun (px, py) -> (px +. x, py +. y)) (integre Data.dt v) in

        Flux.map2 (fun pn vn -> (pn, vn)) p v  
        
      in
    
    let blist = aux (find_briques quadtreeB ((x, y),(dx,dy)))  [] in
    run (Collision.rebond (x,y) (dx, dy) blist (mouse_x, mouse_dx))

    
    let quadtree_update : (quadtree*int) -> balle -> (quadtree * int) Flux.t =
      fun (quadtree,n) ((x, y), (dx, dy)) ->
        
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

          if List.length colliding_briques = 0 || y < 100. then
            (quadtree, n)
          else
            (purge_tree quadtree colliding_briques, n-List.length colliding_briques)
          
        in
          
        Flux.constant (a_supprimer quadtree ((x, y), (dx, dy)))         
        



let score_update : score -> quadtree -> balle -> (score Flux.t * int) = 
  let live_up lives y = 
    if y < 10. then
      (if (lives - 1) = 0 then 
        (0)
      else
        (lives -1))
    else
      lives
  in
  let destrbriqulist quadtree balle=
  let briques = find_briques quadtree balle in

    let rec aux briques l_col =
      match briques with
      | [] -> l_col
      | briquecoord::q ->
          let (a, b) = is_colliding ((fst balle), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (snd balle) in
          if (a, b) <> (0.0, 0.0) then
            aux q (briquecoord::l_col)
          else
            aux q l_col
      in
      aux briques []
  in
  let add_score tree balle = (List.length (destrbriqulist tree balle))
  in
  fun (current_score, lives) tree ((x, y), v) ->
        (Flux.constant(current_score + add_score tree ((x, y), v), live_up lives y),live_up lives y)
    


let rec game_update : etat -> etat Flux.t =
  fun (balle, raquette, score, (quadtreeB,nbBrique)) ->

    let balle_flux = balle_update  raquette balle quadtreeB in

    let raquette_update = 
      let raquette_outside x =
        if(x>(Init.Box.supx +. Init.Box.infx -. float_of_int Init.RaquetteInit.width)) then (Init.Box.supx +. Init.Box.infx -. float_of_int Init.RaquetteInit.width) 
        else if (x<0.) then 0. else x ;
      in
      let balle_up _ =
        match Graphics.mouse_pos () with
          |(xraq,_) -> (raquette_outside(float_of_int xraq),0.)
      in
      Flux.map balle_up (Flux.constant((0.,0.))) in

    let (score_flux,live) = score_update score quadtreeB balle in

    let quadtreeB_flux = quadtree_update (quadtreeB,nbBrique) balle in

    let cond b q s r = 
      
      let (score, lives) = s in
      
      let (nx, ny), (ndx, ndy) = b in

      let quadtree, _ = q in

      let briques = find_briques quadtree ((nx, ny), (ndx, ndy)) in
      

      let rec aux_cond briques balle =
        match briques with
        | [] -> false 
        | briquecoord::q ->
            let (a, b) = is_colliding ((fst balle), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (snd balle) in
            if (a, b) <> (0.0, 0.0) then true else aux_cond q balle
      in

      aux_cond briques ((nx, ny), (ndx, ndy)) 
      || 
      ((ny -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && ((ny -. BalleInit.radius) > (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height -. 5.) && (ndy < 0.) && (nx >= ((fst r))) && nx <= ((((fst r)) +. (float_of_int RaquetteInit.width)))))
      ||
      Collision.contact_x nx ndx
      ||
      Collision.contact_y ny ndy

    in

  
    if(live<>0) && (nbBrique>0) then
      let map4 f i1 i2 i3 i4 = Flux.(apply (apply (apply (apply (constant f) i1) i2) i3) i4) in

      let flux_normal = map4 (fun b r s q -> (b, r, s, q)) balle_flux raquette_update score_flux quadtreeB_flux in

      Flux.unless flux_normal (fun (b, r, s, q) -> cond b q s r) (fun (b, r, s, q) -> game_update (b, r, s, q))
    else 
      (if(nbBrique=0) then (print_endline "vous avez gagné!") else ();
      Flux.vide)





    

      


    







    






