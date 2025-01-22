open Iterator

(* flux de paires (abscisse souris, booléen vrai si bouton appuyé) *)
let mouse =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()

module Collision (F : Frame) = struct
  let contact_x x dx =
    let aux =
      match F.box_x with
      | xmin, xmax -> xmin, xmax
    in
    let xmin, xmax = aux in
    if x > xmax && dx > 0. then true else if x < xmin && dx < 0. then true else false

  let rebond_x x dx = if contact_x x dx then -.dx else dx

  let contact_y y dy =
    let aux =
      match F.box_x with
      | ymin, ymax -> ymin, ymax
    in
    let ymin, ymax = aux in
    if y > ymax && dy > 0. then true else if y < ymin && dy < 0. then true else false

  let rebond_y y dy = if contact_y y dy then -.dy else dy

  let rec unless flux cond f_flux =
    match Flux.uncons flux with
    | None -> Flux.vide
    | Some (a, fl) -> if cond a then f_flux a else Flux.cons a (unless fl cond f_flux)

  let rec run : etat -> etat Flux.t =
    fun ((x, y), (dx, dy)) ->
    let g = 9.81 in
    let a_flux = Flux.constant (0.0, -.g) in
    let v_flux = Flux.map (fun (vx, vy) -> vx +. dx, vy +. dy) (integre F.dt a_flux) in
    let x_flux = Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre F.dt v_flux) in
    unless
      (Flux.map2 (fun x v -> x, v) x_flux v_flux)
      (fun ((x, y), (dx, dy)) -> contact_x x dx || contact_y y dy)
      (fun ((x, y), (dx, dy)) -> run ((x, y), (rebond_x x dx, rebond_y y dy)))
end
