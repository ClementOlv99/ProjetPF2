open Init


let draw_raquette (xpos,vpos) = Graphics.draw_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height;
                         Graphics.set_color Graphics.blue;
                         Graphics.fill_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height

let draw_balle b = 
  match b with
    | ((x,y),(dx,dy)) -> Graphics.draw_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius);
                   Graphics.set_color Graphics.red;
                   Graphics.fill_circle (int_of_float x) (int_of_float y) (int_of_float BalleInit.radius)
    | _ -> ()

let draw_score (s,l) =
  Graphics.moveto 10 22;
  Graphics.set_color Graphics.green;
  Graphics.draw_string ("score : " ^ (string_of_int s));
  Graphics.moveto 10 35;
  Graphics.draw_string ("nombre de vie : " ^ (string_of_int l))

let draw_nbrique n = 
  (Graphics.moveto 10 10;
  (Graphics.draw_string ("nombre de briques : " ^(string_of_int n))))