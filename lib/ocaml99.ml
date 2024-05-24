(* p1 *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs

(* p2 *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

(* p3 *)
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t

(* p4 *)
let length lst =
  let rec aux n = function
    | [] -> n
    | _ :: xs -> aux (n + 1) xs
  in
  aux 0 lst

(* p5 *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] lst

(* p6 *)
let is_palindrome lst = lst = List.rev lst

(* p7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  rev (aux [] lst)

(* p8 *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

(* p9 *)
let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] lst)

(* p10 *)
let encode lst =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] lst)

(* p11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let create_tuple cnt elem = if cnt = 1 then One elem else Many (cnt, elem) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> create_tuple (count + 1) x :: acc
    | hd :: (snd :: _ as tl) ->
      if hd = snd then aux (count + 1) acc tl
      else aux 0 (create_tuple (count + 1) hd :: acc) tl
  in
  List.rev (aux 0 [] l)

(* p12 *)
let decode l =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  aux [] (rev l)

(* p13 *)
let encode lst =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle count x :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 1 (rle count a :: acc) t
  in
  rev (aux 0 [] lst)

(* p14 *)
let rec duplicate = function
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs

(* p15 *)
let replicate lst n =
  let rec prepend n acc x =
  if n = 0 then acc else prepend (n - 1) (x :: acc) x in
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (prepend n acc x) xs in
  