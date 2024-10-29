let rev lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] lst
;;

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many l :: ls -> aux (aux acc l) ls
  in
  List.rev (aux [] lst)
;;

let rec compress = function
  | x :: (y :: _ as ys) -> if x = y then compress ys else x :: compress ys
  | smaller -> smaller
;;

let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | x :: (y :: _ as ys) ->
      if x = y then aux (x :: current) acc ys else aux [] ((x :: current) :: acc) ys
  in
  List.rev (aux [] [] lst)
;;

let encode0 lst = pack lst |> List.map (fun l -> List.length l, List.hd l)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode1 lst =
  List.map
    (function
     | 1, x -> One x
     | n, x -> Many (n, x))
    (encode0 lst)
;;

let decode lst =
  List.fold_left
    (fun acc -> function
      | One x -> x :: acc
      | Many (n, x) -> List.init n (fun _ -> x) @ acc)
    []
    lst
;;

let encode lst =
  let rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux cnt acc = function
    | [] -> []
    | [ x ] -> rle cnt x :: acc
    | x :: (y :: _ as z) ->
      if x = y then aux (cnt + 1) acc z else aux 1 (rle cnt x :: acc) z
  in
  List.rev (aux 1 [] lst)
;;

let duplicate lst = List.fold_right (fun x acc -> x :: x :: acc) lst []

let replicate lst cnt =
  List.fold_left (fun acc x -> acc @ List.init cnt (fun _ -> x)) [] lst
;;

let drop lst n =
  let rec aux i = function
    | [] -> []
    | x :: xs -> if i = n then aux 1 xs else x :: aux (i + 1) xs
  in
  aux 1 lst
;;

let split lst n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | x :: xs -> if i = 0 then List.rev acc, x :: xs else aux (i - 1) (x :: acc) xs
  in
  aux n [] lst
;;

let slice lst a b =
  let rec take n = function
    | [] -> []
    | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs
  in
  let rec drop n = function
    | [] -> []
    | x :: xs -> if n = 0 then x :: xs else drop (n - 1) xs
  in
  take (b - a + 1) (drop a lst)
;;
