module Data = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module TailleBriqueInit = struct
  let width = 60
  let height = 20
end

module RaquetteInit = struct
  let width = 80
  let height = 10
  let ypos = 30
end

type balle = (float * float) * (float * float)
type score = (int * int)
