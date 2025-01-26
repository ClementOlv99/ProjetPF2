module Data : sig
  val dt : float
  val gravity : float
  val rqtFrictionCoeff : float
end

module Box : sig
  val marge : float
  val infx : float
  val infy : float
  val supx : float
  val supy : float
end

module TailleBriqueInit : sig
  val width : int
  val height : int
end

module RaquetteInit : sig
  val width : int
  val height : int
  val ypos : int
end

module BalleInit : sig
  val radius : float
end

module TabBriquesInit : sig
  val score : int
  val nbBrique : int
end

val raquette_outside : float -> float
