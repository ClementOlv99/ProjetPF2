open Quadtree

let draw_briques : tree -> unit = 
  fun tree ->
    match tree with
      | Leaf (_,co) -> 
        match co with
          | None -> ()
          | (x1,y1) ->  Graphics.draw_rect (int_of_float x1) (int_of_float y1) TailleBriqueInit.width TailleBriqueInit.height;
                        Graphics.set_color red;
                        Graphics.fill_rect (int_of_float x1) (int_of_float y1) TailleBriqueInit.width TailleBriqueInit.height
      | Node (_,t1,t2,t3,t4) ->  draw_briques t1;
                                 draw_briques t2;
                                 draw_briques t3;
                                 draw_briques t4

let draw_raquette xpos = Graphics.draw_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height;
                         Graphics.set_color blue;
                         Graphics.fill_rect (int_of_float xpos) RaquetteInit.ypos RaquetteInit.width RaquetteInit.height

let draw_balle =