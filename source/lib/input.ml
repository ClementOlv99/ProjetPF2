open Iterator

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
    
end
