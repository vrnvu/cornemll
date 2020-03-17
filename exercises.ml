let rec mul = function
  | [] -> 1
  | x::xs -> x * mul xs
;;

let rec concat = function
  | [] -> ""
  | x::xs -> x ^ (concat xs)
;;

let f1 = function
  | "bigred"::xs -> true
  | _ -> false
;;

let f2 = function
  | a::b::c::d::[] -> true
  | a::b::[] -> true
  | _ -> false
;;

let f3 = function
  | a::b::_ -> a = b
  | _ -> false
;;

 let get4 list = if List.length list < 5 
  then 0
  else List.nth list 4
;;

let descendent_sort list = List.rev (List.sort Stdlib.compare list)
;;

let descendent_sort list = 
  list 
  |> List.sort Stdlib.compare 
  |> List.rev
;;

let last list = 
  list 
  |> List.rev 
  |> List.hd
;;

let has_zero = List.exists (fun a -> a = 0);;

let take n list = 
  let rec rec_take n list result = if n = 0
    then result
    else
      let head = (List.hd list) in
      let tail = (List.tl list) in
      rec_take (n - 1) (tail) (List.cons head result)
  in if List.length list < (n) 
    then []
    else rec_take (n) list []
;;

let drop n list = 
  let rec rec_drop n list = if n = 0
    then list
    else rec_drop (n - 1) (List.tl list)
  in if List.length list < n
  then []
  else rec_drop n list
;;

let is_unimodal list = 
  let rec is_decreasing list = match list with
    | [] -> true
    | x::[] -> true
    | x::y::[] -> x > y
    | x::xs -> if x < List.hd xs then false else is_decreasing xs in 
  let rec is_growing list = match list with
    | [] -> true
    | x::[] -> true
    | x::y::[] -> true
    | x::xs -> if x < List.hd xs then is_growing xs else is_decreasing list in
match list with
  | [] -> true
  | x::[] -> true
  | _ -> is_growing list
;;

let powerset set = []
;;

let rec repeat f n x = match n with
  | 0 -> x
  | n -> repeat f (n - 1) (f x)
;;

let sum_cube_odd n =
  let range = (0 -- n) in
  let odds = List.filter (even) range in
  let cubes = List.map (fun a -> a * a * a) odds in
  List.fold_left (+) 0 (cubes)
;;

let sum_cube_even n =
  0 -- n
  |> List.filter (even)
  |> List.map (fun a -> a * a * a)
  |> List.fold_left (+) 0
;;

let exists p list = 
  let rec iter p = function
    | [] -> false
    | x :: xs -> if p x then true else iter p xs
  in iter p list
;;

let exists p list = 
  List.fold_left
    (fun init a -> if p a then true || init else false || init)
    false
    list
;;

let exists p list = 
  List.map p list
  |> List.filter (fun a -> a = true)
  |> List.length
  |> (fun l -> l > 0)
;;

let budget initial expenses = 
  let rec iter initial = function
    | [] -> initial
    | x :: xs -> iter (initial - x) xs
in iter initial expenses;;

let budget initial = List.fold_left (-) initial;;

let budget initial expenses = 
  List.fold_right (fun a b -> b - a) expenses initial;;

let uncurried_nth (lst,n) = List.nth lst n;;
let uncurried_append (a,b) = List.append a b;;
let uncurry f (a, b) = f a b;;
let curry f (a, b) = f a b;;

let map_composition f g = List.map (fun a -> f (g a));;
let map_composition f g = List.map (fun a -> a |> g |> f);;

let f1 list = List.filter (fun a -> (String.length a) > 3) list;;

type student = { first_name : string ; last_name : string ; gpa : float };;

let make_student name last gpa = {
  first_name = name;
  last_name = last;
  gpa = gpa
};;

let get_name student = student.first_name;;

let safe_hd = function
  | [] -> None
  | x::xs -> Some x
;; 

let rec safe_tl = function
  | [] -> None
  | [x] -> Some x
  | x::xs -> safe_tl xs
;;

module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end;;

module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty  : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings 
   * as [d] and also a mapping from [k] to [v], even if [k] was already 
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t

  (* [lookup k d] returns the value associated with [k] in [d].  
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup  : 'k -> ('k,'v) t -> 'v
end
