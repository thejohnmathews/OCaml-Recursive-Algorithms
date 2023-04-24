(* SDE 2 CPSC 3520 John Mathews*)

(* 2.1 first_duplicate - first integer in list with first duplicate*)
let rec first_duplicate numlist = 
  match numlist with
  (* base case: empty list return -10000 *)
  | [] -> -10000 
  (* search for duplicate, take 'value' from list then compare to 'rest' of the numbers*)
  | value :: rest ->
    if List.mem value rest then value
    else first_duplicate rest;;

(* 2.2 first_nonrepeating - first item that is not in the list more than once*)
let rec helper_nonrepeating numlist = 
  match numlist with
  (* base case: 1st arg: list of remaining vals, 2nd arg: values seen/not repeated*)
  | ([], numlist) -> -10000
  (* check for nonrepeating value *)
  | (value::rest, numlist) ->
    if not(List.mem value rest || List.mem value numlist) then value
    else helper_nonrepeating(rest, value::numlist);;

let rec first_nonrepeating = function
    (numlist) ->

      (* start off by sending tuple *)
      helper_nonrepeating(numlist, []);;

(* 2.3 sumOfTwo - return boolean if a+b (!)= v *)

  
let rec sumOfTwo a b v =
  match (a, b, v) with
  | ([], [], v) -> false
  | ([], b, v) -> false
  | (a, b, v) -> 
    if 

