(* The prime factors of 13195 are 5, 7, 13 and 29.
 * What is the largest prime factor of the number
 * 600851475143?
 *)

open LargeInt;

local
  fun fact 1 m = []
    | fact n m =
      if n mod m = 0
      then m :: (fact (n div m) m)
      else fact n (m+2)
in
fun factors n =
    fact n 3
end;


val solution = factors(600851475143);

