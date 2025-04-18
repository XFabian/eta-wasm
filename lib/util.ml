(* Rounds x to nearest multiple of n *)
let round_to_n x n =
  match x with
  | x when x mod n = 0 -> x
  | x when x < 0 -> x - n - (x mod n)
  | x -> x + n - (x mod n)