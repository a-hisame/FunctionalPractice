
(* take: *)
let rec take n xs = match (n, xs) with
  | (_, []) -> []
  | (0, _) -> []
  | (n, x :: xs) -> x :: take (n-1) xs

(* drop *)
let rec drop n xs = match (n, xs) with
  | (_, []) -> []
  | (0, xs) -> xs
  | (n, _ :: xs) -> drop (n-1) xs

(* tupled take: tupled function expresses pattern match directly. *)
let rec t_take = function
  | (_, []) -> []
  | (0, _) -> []
  | (n, x :: xs) -> x :: take (n-1) xs

(* splitAt function in haskell *)
let splitAt n xs = (take n xs, drop n xs)

(* Practice 4.1: halve *)
let halve xs = splitAt (List.length xs / 2) xs

 
