module DependentMap

module T = FStar.Tactics
module Map = FStar.OrdMap
module L = FStar.List.Tot
open HTTP.Server.Helpers

unfold let field = ( t: Type & t )
type t (key: eqtype) = key -> field

let empty #kT: t kT = fun _ -> (|unit, ()|) 

unfold let typeAt #kT (map: t kT) (key: kT): Type
  = let (| t, _ |) = map key in t

let get (#kT: eqtype) (map: t kT) (key: kT): typeAt map key
  = let (| _, v |) = map key in v

let constraints_has_key (#kT:eqtype) (k: kT) cl
  = normalize_term (L.fold_left (fun p (k', t') -> p && (k' = k)) true cl)

let type_of_constraints (#kT:eqtype) (k: kT) cl
  : option Type
  = normalize_term (
    match L.find (fun (k', t') -> (k' = k)) cl with
    | Some (_, x) -> Some x
    | None -> None
  )

let constraints (#kT: eqtype) = list_nodup_mod_prop #kT #(kT*Type) fst

let only_changes_are
  (#kT: eqtype) (map0 map1: t kT)
  (constraints: constraints)
  = forall k. 
      match type_of_constraints k constraints with
      | Some typ -> typeAt map1 k == typ
      | None     -> typeAt map1 k == typeAt map0 k

let set (#kT: eqtype) #vT (map: t kT) (key: kT) (value: vT)
  : (map': t kT {
        only_changes_are map map' [key, vT]
    })
  = fun k ->  if k = key
           then (| vT, value |)
           else map k

let all (f: 'a -> Type0) (l: list 'a): Type0 =
  L.fold_left l_and True (L.map f l)
let some (f: 'a -> Type0) (l: list 'a): Type0 =
  L.fold_left l_or False (L.map f l)

let (+:) (#kT: eqtype) (l: constraints) (v:kT*Type): constraints
  = insert_list_nodup_mod_prop _ l v

let (++) (#kT: eqtype) (l l': constraints #kT): constraints
  = L.fold_left (+:) l l'

let list_to_constraints (#kT: eqtype) l =
  L.fold_left (+:) [] l

let has1 (#kT: eqtype) key typ (map: t kT)
  = typeAt map key == typ

let has (#kT: eqtype) (cl: constraints) (map: t kT)
  = all (fun (k, t) -> has1 k t map) cl


let normalize_has_constraints
  c1 c2 map
  : Lemma
    (requires
        normalize (has c1 map)
        ==> normalize (has c2 map)
      )
    (ensures has c1 map ==> has c2 map)
    [SMTPat (has c1 map ==> has c2 map)]
  = assert (normalize (has c1 map) == has c1 map) by (T.compute ());
    assert (normalize (has c2 map) == has c2 map) by (T.compute ())

let normalize_same
  c1 map
  : Lemma
    (requires normalize (has c1 map))
    (ensures has c1 map)
    [SMTPat (normalize (has c1 map))]
  = assert_norm (normalize (has c1 map) == has c1 map)

let only_changes_implies
  (#kT: eqtype)
  (map0 map1: t kT)
  (constraintsMap0 newOnes: constraints)
  : Lemma
      (requires 
          has constraintsMap0 map0
        /\ only_changes_are map0 map1 newOnes
      )
      (ensures
        has (constraintsMap0 ++ newOnes) map1
      )
      [SMTPat (
        has constraintsMap0 map0
        /\ only_changes_are map0 map1 newOnes
      )]
  = admit ()


let setMany (#kT: eqtype) (map: t kT)
  (tuples: list (kT * ((vT: Type) & vT)))
  : (r: t kT {
        forall cl. has cl map ==> has (cl ++ 
          (list_to_constraints
            (L.map (fun (k, (|t, _|)) -> (k, t)) tuples))
        )  r
    })
  = let f map (k, (|t, v|)) = set #kT #t map k v in
    L.fold_left (admit (); f) map tuples


let append_lemma l l' data
  : Lemma 
    (requires has (l ++ l') data)
    (ensures has l' data) 
  [SMTPat (has (l ++ l') data)]
  = admit ()
  
let (>>) (l2 l1: constraints) =
  all (fun constraint ->
    some (fun constraint' -> constraint == constraint') l2
  ) l1

let set_add_lemma'
  (#kT: eqtype) #vT
  (map: t kT)
  (cl: constraints)
  (key: kT) (value: vT)
  (_:unit {has cl map})
  : Lemma (
    has (cl +: (key, vT)) (set map key value) 
  )
  = admit ()

let set_add_lemma''
  (#kT: eqtype)
  (map: t kT)
  (cl: constraints)
  (key: kT)
  (_:unit {has cl map})
  : Lemma (
    forall vT (value: vT). has (cl +: (key, vT)) (set map key value) 
  )
  = admit ()
  
// let set_add_lemma'
//   (#kT: eqtype) #vT
//   (map: t kT)
//   (cl: constraints)
//   (c2: constraints)
//   (key: kT) (value: vT)
//   (_:unit {has cl map})
//   : Lemma (
//      has (cl ++ (key, vT)) (set map key value) 
//   )
//   = admit ()

let preseves (#kT: eqtype) #comp #vT (constraints: Map.ordmap kT Type comp) (newOnes: list kT) (map: t kT)
  = forall k. ~(L.mem k newOnes) ==> Map.select k constraints == Some (typeAt map k)

let set_conserves_lemma
  (#kT: eqtype) #vT
  #comp
  (map: t kT)
  (key: kT) (value: vT)
  : Lemma (
    forall k p.
      k <> key ==> (p (map k) <==> p ((set map key value) k))
  )
  = ()
 
let set_conserves_lemma2
  (#kT: eqtype) #vT
  (key': kT) (vT': Type)
  (map: t kT)
  (_: _{typeAt map key' == vT'})
  (key: kT) (value: vT)
  : Lemma (
    key <> key' ==> 
    typeAt (set map key value) key' == vT'
  )
  = ()
  
let set_add_lemma
  (#kT: eqtype) #vT
  (map: t kT)
  (key: kT) (value: vT)
  : Lemma (
    typeAt (set map key value) key == vT
  )
  = ()
  
