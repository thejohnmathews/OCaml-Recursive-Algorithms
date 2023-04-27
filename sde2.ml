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

let rec first_nonrepeating = function (numlist) ->
      (* start off by sending tuple *)
      helper_nonrepeating(numlist, []);;

(* 2.3 sumOfTwo - return boolean if a+b (!)= v *)
let rec sumOfTwo (a, b, v) =
  match (a, b) with 
  (* base case: if lists are empty there is not a pair to make it true*)
  | ([], _) -> false
  | (_, []) -> false
  (* if lists have values, check a + b = v*)
  | (aHead :: aTail, bHead :: bTail) ->
    if aHead + bHead = v then true
    (* call function again with tails of both lists*)
    else sumOfTwo(aTail, b, v) || sumOfTwo(a, bTail, v);;

(* 2.4 cyk_sublists - returns all integer pairs that could add up to n *)
let rec helper_cyk (limit, i, numlist) =
  match (limit, i, numlist) with
  (* add next pairing*)
  | (limit, i, numlist) when i - limit > 0 -> helper_cyk(limit + 1, i, (abs (i - limit), limit) :: numlist)
  (*base case: return all pairings*)
  | _ -> numlist 

let rec cyk_sublists number =
  (*send off triple*)
  helper_cyk(1, number, [])