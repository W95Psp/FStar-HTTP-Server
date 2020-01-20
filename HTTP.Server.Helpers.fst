module HTTP.Server.Helpers

open FStar.String
module L = FStar.List.Tot

let rec trimLeftList (keep: 'a -> bool) (s: list 'a): list 'a =
  match s with
  | [] -> []
  | h::tl -> if keep h then h::tl else trimLeftList keep tl
let trimRightList (keep: 'a -> bool) (s: list 'a): list 'a =
  L.rev (trimLeftList keep (L.rev s))
let trimList (keep: 'a -> bool) (l: list 'a) =
  trimRightList keep (trimLeftList keep l)
let notSpace c = match c with|'\t'|' '|'\n'|'\r'->false|_->true
let mkListFunStrFun f s = string_of_list (f (list_of_string s))
let trimRight = mkListFunStrFun (trimRightList notSpace)
let trimLeft = mkListFunStrFun (trimLeftList notSpace)
let trim = mkListFunStrFun (trimList notSpace)



let option_bind #a #b (v: option a) (f: a -> option b) = match v with
    | Some v -> f v
    | None   -> None

let rec set key value (l: list (string * string))
  = match l with
  | [] -> [(key, value)]
  | (k,v)::tl -> if k = key
               then (k, value)::tl
               else (k,v)::set key value tl

let rec contains_helper b (n h: list char): Tot _ (decreases h)
      = match n, h with
        | [], _ -> true
        | _, [] -> false
        | hn::tn, hh::th ->
          if hh = hn
          then contains_helper false tn th
          else (
            if b
            then contains_helper true n th
            else false
          )

let contains (needle: string) (haystack: string): bool
  = let needle = list_of_string needle in
    let haystack = list_of_string haystack in
    contains_helper true needle haystack

let rec nodup_mod_prop_helper (#a: eqtype) #b (f: b -> a)
  (seen: list a) (l: list b)
  : Tot bool (decreases l)
  = match l with
  | [] -> true
  | hd::tl -> let hd = f hd in
            if L.mem hd seen
            then false
            else nodup_mod_prop_helper f (hd::seen) tl
            
let nodup_mod_prop (#a: eqtype) #b (f: b -> a) (l: list b)
  = nodup_mod_prop_helper #a #b f [] l

let list_nodup_mod_prop (#a: eqtype) #b (f: b -> a) = l: list b {nodup_mod_prop #a #b f l}

let rec insert_list_nodup_mod_prop (#a: eqtype) #b (f: b -> a) (l: list_nodup_mod_prop f) (v: b)
  : list_nodup_mod_prop f
  = match l with
  | [] -> [v]
  | hd::tl -> let tl' = insert_list_nodup_mod_prop f (admit (); tl) v in
            if f hd = f v
            then tl'
            else hd::tl'

let rec exists_in_list  (eq: 'a -> 'a -> bool) (v: 'a) (l: list 'a) =
  match l with
  | [] -> false
  | hd::tl -> 
    if eq hd v then true else exists_in_list eq v tl

let rec rm_dup_in_list_helper #a (eq: a -> a -> bool) (l: list a) (seen: list a)
    : list a
    = match l with
    | [] -> []
    | hd::tl -> if exists_in_list eq hd seen
              then rm_dup_in_list_helper eq tl seen
              else hd::rm_dup_in_list_helper eq tl (hd::seen) 

let rm_dup_in_list (eq: 'a -> 'a -> bool) (l: list 'a) =
  rm_dup_in_list_helper eq l [
]

let fromOption (o: option 'a) (d: 'a): 'a 
  = match o with
  | Some x -> x
  | None -> d


let split_in_2 (cut: char) (s: string): string * string = 
  let cut = [cut] in
  match String.split cut s with
  | hd::tl -> hd, String.concat (string_of_list cut) tl
  | [hd] -> hd, ""
  | _ -> "", ""


let parseUrlEncodedString body =
  (L.map (split_in_2 '=') (split ['&'] body))

let mapOption (f: 'a -> 'b) (v: option 'a) =
  let bind = option_bind in
  v <-- v;
  Some (f v)

