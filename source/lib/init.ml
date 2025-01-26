module Data = struct
(** Données liées à la physique du jeu. 
    *)
  let dt = 0.01 (* 60 Hz *)
  let gravity = 40.
  (** Gravité, en m.s⁻² 
      *)
  let raquetteAngle = 45.
  (** Angle maximal de la raquette aux extrémités 
      *)
  let angleSpeedThreshold = 45.
  (** Vitesse à partir de laquelle la raquette envoie la balle à l'angle maximal 
      *)
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

module BalleInit = struct
  let radius = 10.
end

module TabBriquesInit = struct 
  let score = 50 
  let nbBrique = 10 
end

let raquette_outside x =
  if(x>(Box.supx +. Box.infx -. float_of_int RaquetteInit.width)) then (Box.supx +. Box.infx -. float_of_int RaquetteInit.width) 
  else if (x<0.) then 0. else x ;