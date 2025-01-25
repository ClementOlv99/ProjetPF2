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
      (fun () ->
        let x, _ = Graphics.mouse_pos () in
        Some ((float_of_int x, 0.), ()))
      ()

let balle_update : raquette -> balle -> quadtree -> balle Flux.t =
  fun (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtreeB -> 

    let g = 9.81 in


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

    
    let quadtree_update : quadtree -> balle -> (quadtree * int) Flux.t =
      fun quadtree ((x, y), (dx, dy)) ->
        
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
      

      let rec aux_cond briques balle =
        match briques with
        | [] -> false 
        | briquecoord::q ->
            let (a, b) = is_colliding ((fst balle), BalleInit.radius) (briquecoord, ((float_of_int TailleBriqueInit.width), (float_of_int TailleBriqueInit.height))) (snd balle) in
            if (a, b) <> (0.0, 0.0) then true else aux_cond q balle
      in

      if (aux_cond briques ((nx, ny), (ndx, ndy))  || ((ny -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (ndy < 0.) && (nx >= float_of_int (fst (Graphics.mouse_pos ())) && nx <= ((float_of_int (fst (Graphics.mouse_pos ()))) +. (float_of_int RaquetteInit.width)))  || Collision.contact_y ny ndy || Collision.contact_y ny ndy )) then
        (print_endline(string_of_bool (aux_cond briques ((nx, ny), (ndx, ndy))) );
        print_endline(string_of_bool (((ny -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (ndy < 0.) && (nx >= float_of_int (fst (Graphics.mouse_pos ())) && nx <= ((float_of_int (fst (Graphics.mouse_pos ()))) +. (float_of_int RaquetteInit.width))))));
        print_endline(string_of_bool (Collision.contact_x nx ndx ));
        print_endline(string_of_bool (Collision.contact_y ny ndy));

        aux_cond briques ((nx, ny), (ndx, ndy)) 
        || 
        ((ny -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (ndy < 0.) && (nx >= float_of_int (fst (Graphics.mouse_pos ())) && nx <= ((float_of_int (fst (Graphics.mouse_pos ()))) +. (float_of_int RaquetteInit.width))))
        ||
        Collision.contact_y ny ndy)
      else
        aux_cond briques ((nx, ny), (ndx, ndy)) 
        || 
        ((ny -. BalleInit.radius < (float_of_int RaquetteInit.ypos +. float_of_int RaquetteInit.height)) && (ndy < 0.) && (nx >= float_of_int (fst (Graphics.mouse_pos ())) && nx <= ((float_of_int (fst (Graphics.mouse_pos ()))) +. (float_of_int RaquetteInit.width))))
        ||
        Collision.contact_y ny ndy

    in

  


    let map4 f i1 i2 i3 i4 = Flux.(apply (apply (apply (apply (constant f) i1) i2) i3) i4) in

    let flux_normal = map4 (fun b r s q -> (b, r, s, q)) balle_flux raquette_flux score_flux quadtreeB_flux in
    print_endline("test30000000000000");
    Flux.unless flux_normal (fun (b, r, s, q) -> cond b q s r) (fun (b, r, s, q) -> game_update (b, r, s, q))




    

      


    







    






