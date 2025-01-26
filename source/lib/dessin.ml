open Init


let draw_raquette (xpos,vpos) = Graphics.draw_rect (int_of_float (raquette_outside (float_of_int (fst (Graphics.mouse_pos ()))))) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height;
                         Graphics.set_color Graphics.blue;
                         Graphics.fill_rect (int_of_float (raquette_outside (float_of_int (fst (Graphics.mouse_pos ()))))) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height

let draw_balle b = 
  match b with
    | ((x,y),(dx,dy)) -> Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius);
                   Graphics.set_color Graphics.red;
                   Graphics.fill_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius)
    | _ -> ()