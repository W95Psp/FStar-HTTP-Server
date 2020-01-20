module Data.Heterogeneous.Map

module T = FStar.Tactics
module L = FStar.List.Tot


module S = FStar.Set
module M = FStar.Map

module OS = FStar.OrdSet
module OM = FStar.OrdMap
open FStar.FunctionalExtensionality


type constraints (kT: eqtype) = M.t kT Type

type hMap (kT: eqtype) = M.t kT ( t: Type & v: t )

unfold let selectTuple (#kT: eqtype) (map: hMap kT) (k: kT): ( t: Type & t ) = M.sel map k

unfold let selectType (#kT: eqtype)
  map (k: kT {M.contains map k})
  : Type = let (| t, v |) = M.sel map k in t

unfold let select (#kT: eqtype)
  map (k: kT {M.contains map k})
  = let (| _, v |) = M.sel map k in v

unfold let upd (map: constraints _) = M.upd map


// let cmp: OM.cmp string = admit (); fun x y -> x `String.compare` y > 0

(* CONSTRAINTS
These are maps from string to types.
*)
// type constraints = OM.ordmap string Type cmp

// let addConstraint (constraints: constraints) (key, typ)
//   = OM.update key typ constraints

// let addConstraints (constraints: constraints) (l: list (string * Type))
//   = List.fold_left addConstraint constraints l

// let emptyConstraints: constraints = OM.empty

// let toConstraints (l: list (string * Type))
//   = addConstraints emptyConstraints l

// (* HETEROGENEOUS MAPS
// *)
// type hMap = OM.ordmap string ( t: Type & value: t ) cmp

// let select key (map: hMap) = let (| _, v |) = (OM.select key map)
//                              in v


