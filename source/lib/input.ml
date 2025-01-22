open Iterator
open Game

(* flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()


module Collision = struct

  

  let rec unless flux cond f_flux =
    match Flux.uncons flux with
    | None -> Flux.vide
    | Some (a, fl) -> if cond a then f_flux a else Flux.cons a (unless fl cond f_flux)

  let rec run : etat -> etat Flux.t =

    fun ((x0,y0),(dx0,dy0)) -> 

      let a = Flux.constant (0., -9.81) in

      let v = Flux.map (fun (a , b) -> (a +. dx0, b +. dy0)) (integre F.dt a) in

      let p = Flux.map (fun (a , b) -> (a +. x0, b +. y0)) (integre F.dt v) in

      Flux.map2 (fun pn vn -> (pn, vn)) p v
    
end
