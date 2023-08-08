let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t
let%test "last_some" = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test "last_none" = last [] = None

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let%test "last_two_some" = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test "last_two_none" = last_two [ "a" ] = None

let rec nth n = function
  | [] -> None
  | h :: t -> if n = 0 then Some h else nth (n - 1) t

let%test "nth_some" = nth 2 [ "a"; "b"; "c"; "d"; "e" ] = Some "c"
let%test "nth_none" = nth 2 [ "a" ] = None

(* let rec len = function [] -> 0 | [ _ ] -> 1 | _ :: t -> 1 + len t *)
let length list =
  let rec aux n = function [] -> n | _ :: t -> aux (n + 1) t in
  aux 0 list

let%test "len_nonzero" = length [ "a"; "b"; "c" ] = 3
let%test "len_zero" = length [] = 0

let rev list =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] list

let%test "reverse" = rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]
let is_palindrome list = list = List.rev list
let%test "palindrome_true" = is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true
let%test "palindrome_false" = is_palindrome [ "a"; "b" ] = false

type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list)

let%test "flatten" =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | x -> x

let%test "compress" =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]

let pack list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] list)

let%test "pack" =
  pack
    [
      "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e";
    ]
  = [
      [ "a"; "a"; "a"; "a" ];
      [ "b" ];
      [ "c"; "c" ];
      [ "a"; "a" ];
      [ "d"; "d" ];
      [ "e"; "e"; "e"; "e" ];
    ]

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)

let%test "encode" =
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]

type 'a rle = One of 'a | Many of int * 'a

let encode2 list =
  let create_tuple count elem =
    if count = 1 then One elem else Many (count, elem)
  in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] list)

let%test "encode2" =
  encode2
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]

let encode3 list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle count x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t
  in
  List.rev (aux 0 [] list)

let%test "encode3" =
  encode3
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]

let decode list =
  let rec unpack acc n x = if n = 0 then acc else unpack (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (unpack acc 1 x) t
    | Many (n, x) :: t -> aux (unpack acc n x) t
  in
  aux [] (List.rev list)

let%test "decode" =
  decode
    [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]
  = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

let duplicate list =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: h :: acc) t in
  aux [] (List.rev list)

let%test "duplicate" =
  duplicate [ "a"; "b"; "c"; "c"; "d" ]
  = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]

let replicate list n =
  let rec unpack acc n x = if n = 0 then acc else unpack (x :: acc) (n - 1) x in
  let rec aux acc = function [] -> acc | h :: t -> aux (unpack acc n h) t in
  aux [] (List.rev list)

let%test "replicate" =
  replicate [ "a"; "b"; "c" ] 3
  = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]

let drop list n =
  let rec aux curr = function
    | [] -> []
    | h :: t -> if curr = n then aux 1 t else h :: aux (curr + 1) t
  in
  aux 1 list

let%test "drop" =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
