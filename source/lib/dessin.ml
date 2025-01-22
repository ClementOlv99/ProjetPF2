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

(*module TailleBriqueInit = struct
  let width = 60
  let height = 20
end

let draw_briques tree = 
  match tree with 
    | Leaf (l,co) -> 
      match co with
        | None -> ()
        | (x1,y1) ->  Graphics.draw_rect x1 y1 width height; 
                      Graphics.synchronize()
    | Node (l,t1,t2,t3,t4) ->  draw_briques t1;
                               draw_briques t2;
                               draw_briques t3;
                               draw_briques t4

*)