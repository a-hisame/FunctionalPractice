(* alpha to int *)
let let2int c = match (Char.code c) - (Char.code 'a') with
 | x when 0 <= x && x < 26 -> x
 | _ -> -1

(* int to alpha *)
let int2let = function
 | n when 0 <= n && n < 26 -> Char.chr (n + Char.code 'a')
 | _ -> ' '

(* is lower alphabet *)
let is_lower c = match (Char.code c) - (Char.code 'a') with
 | x when 0 <= x && x < 26 -> true
 | _ -> false

(* abs :: int -> int *)
let abs n = if n >= 0 then n else -n

(* str2list :: string -> char list *)
let str2list s = 
 let rec smap s n =
   if n <= 0 then []
   else s.[0] :: smap (String.sub s 1 (n-1)) (n-1)
 in
   smap s (String.length s) 

(* list2str :: char list -> string *)
let list2str cs = 
 String.concat "" (List.map (fun c -> String.make 1 c) cs)

(* shift_mod :: int -> int *)
let shift_mod n d = 
 if n >= 0 then n mod d 
 else (d + (n mod d)) mod d

(* shift code *)
let shift n c = match (n,c) with 
 | (n, c) when is_lower c -> int2let(shift_mod (let2int c + n) 26)
 | _ -> c

(* encode to caesar *)
let encode n s = 
 let xs = str2list s in
 let shifted = List.map (shift n) xs in
  list2str shifted

(* decode to caesar *)
let decode n s = encode (-n) s

(* ----- test -------------------- *)

(* shift test *)
let test1 = shift 1 'a' = 'b'
let test2 = shift 25 'a' = 'z'
let test3 = shift 26 'a' = 'a'
let test4 = shift (-1) 'b' = 'a'
let test5 = shift (-1) 'a' = 'z'
let test6 = shift (-25) 'a' = 'b'
let test7 = shift (-52) 'a' = 'a'

(* encode test *)
let enc_test1 = encode 1 "" = ""
let enc_test2 = encode 2 "abc" = "cde"
let enc_test3 = encode 25 "abc" = "zab"
let enc_test4 = decode 10 (encode 10 "yzab") = "yzab"
let enc_test5 = encode 30 (decode 30 "yzab") = "yzab"



