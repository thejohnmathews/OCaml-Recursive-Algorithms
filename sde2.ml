(* SDE 2 CPSC 3520 John Mathews*)

(* 2.1 first_duplicate - first integer in list with first duplicate*)
let rec first_duplicate numlist =
  match numlist with
  (* base case: empty list return -10000*)
  | [] -> -10000 
  (* search for duplicate, take 'value' from list then compare to 'rest' of the numbers*)
  | value :: rest ->
    if List.mem value rest then value
    else first_duplicate rest

(*2.2 first_nonrepeating - first item that is not in the list more than once*)
