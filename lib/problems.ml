let inc x = x + 2

(* P1: Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
| [] -> None
| [x] -> Some x
| _ :: t -> last t

let%test _ = Option.equal (=) (last [5]) (Some 5)
let%test _ = Option.equal (=) (last [5; 6]) (Some 6)
let%test _ = Option.is_none (last [])
