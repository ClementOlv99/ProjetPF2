open Quadtree

module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

(*paramétres*)

module TailleBriqueInit = struct
  let width = 60
  let height = 20
end

module RaquetteInit = struct
  let width = 80
  let height = 10
  let ypos = 30
end

let draw_briques tree = 
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
