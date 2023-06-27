let inc x = x + 2

(* P1: Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
| [] -> None
| [x] -> Some x
| _ :: t -> last t

let%test _ = Option.equal (=) (last [5]) (Some 5)
let%test _ = Option.equal (=) (last [5; 6]) (Some 6)
let%test _ = Option.equal (=) (last ["a" ; "b" ; "c" ; "d"]) (Some "d")
let%test _ = Option.is_none (last [])

(* P2: Find the last but one (last and penultimate) elements of a list. *)
let rec last_two = function
  | [] | [_] -> None
  | x :: y ::[] -> Some (x, y)
  | _ :: t -> last_two t

let%test _ = Option.equal (=) (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d"))
let%test _ = Option.equal (=) (last_two [1; 2]) (Some (1, 2))
let%test _ = Option.is_none (last_two [1])
let%test _ = Option.is_none (last_two [])
