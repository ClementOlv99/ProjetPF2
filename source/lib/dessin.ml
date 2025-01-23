open Init


let draw_raquette xpos = Graphics.draw_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height;
                         Graphics.set_color Graphics.blue;
                         Graphics.fill_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height

let draw_balle b = 
  match b with
    | ((x,y),r) -> Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float r)
    | _ -> ()