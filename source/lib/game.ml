open Quadtree
open Iterator
open Input
open Init
open Dessin

(*etat de la partie*)

type raquette = (float * float)
type balle = (float * float) * (float * float)
type score = (int * int)
type etat = balle * raquette score * (Quadtree.tree * int)

let game_init liste_brique = 

  let balle = ((Box.supx/.2., Box.supy/.2.), (0., 0.)) in

  let raquette = mouse in

  let score = 0, 3 in

  let quadtree = Quadtree.create_tree (Box.supx, Box.supy/2) liste_brique in

  (balle, raquette, score, (quadtree, List.length liste_brique))



let raquette_update = 
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let dx = (x -. prev_x) /. Init.dt in
      Some ((float_of_int x, dx, Graphics.button_down ()), x))
    ()

let balle_update raquette Flux.t -> palette -> ball -> Quadtree.tree -> ball Flux.t =
  fun raquette_flux
    (mouse_x, mouse_dx)
    ((x, y), (dx, dy))
    quadtree -> 

    let 

    let ndx = rebond_x br_qtree (x, y) (dx, dy) in

    let ndy = rebond_y br_qtree mouse_x (x, y) (dx, dy) in

    let a_flux = Flux.constant (0.0, -9.81) in

    let v_flux = Flux.map (fun (vx, vy) -> vx +. ndx, vy +. ndy) (integre PhysicsInit.dt a_flux) in
    let x_flux =
      Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre PhysicsInit.dt v_flux)
    in
    let is_launched_flux = Flux.constant new_is_launched in

    Flux.map3 (fun x v b -> x, v, b) x_flux v_flux is_launched_flux



let game_update etat -> etat Flux.t = 








    






