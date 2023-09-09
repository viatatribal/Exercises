(* By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
 * we can see that the 6th prime is 13.
 *
 * What is the 100001st prime number?
 *)
local
  (* Check if n is divisible by n prime less than n *)
  fun isPrime n [] = true
    | isPrime n (x::xs) =
      if n mod x = 0
      then false
      else isPrime n xs;
in
fun findPrime n size xs =
    if isPrime n xs
    then
      if size = 10000
      then n
      else findPrime (n+2) (size+1) (n::xs)
    else
      findPrime (n+2) size xs;
end;

val solution = findPrime 3 1 [2];
