(* safetail *)
let safetail xs = match xs with
 | [] -> []
 | _ :: xs -> xs

(* last arguments used in function pattern match. *)
let safetail2 = function
 | [] -> []
 | _ :: xs -> xs


