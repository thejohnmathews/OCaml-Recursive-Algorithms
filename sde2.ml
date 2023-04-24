(* SDE 2 CPSC 3520 John Mathews*)

(* 2.1 first_duplicate - first integer in list with first duplicate*)
let rec first_duplicate lst =
  match lst with
  | [] -> -1000 (* Base case: empty list, return 0 *)
  | x :: xs ->
    (* Check if x appears again in the list *)
    if List.mem x xs then x
    (* Otherwise, recurse on the remaining list *)
    else first_duplicate xs

val first_duplicate : int List -> int = <fun>
