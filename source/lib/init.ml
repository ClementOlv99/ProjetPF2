module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

type balle = (float * float) * (float * float)
type raquette = float
type score = (int * int)

(*etat de la partie*)
type etat = balle * raquette * score * (Quadtree.tree * int)