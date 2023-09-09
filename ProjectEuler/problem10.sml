(*The sum of the primes below 10 is
 * 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 *)

open LargeInt;
local
  fun intSqrt n =
      fromInt
          (Real.floor
               (Real.Math.sqrt(Real.fromLargeInt n)));
  (* Check if n is divisible by n prime less than n *)
  fun isPrime n  =
      let
        fun prime' f r =
            if f > r
            then true
            else if n mod f = 0
            then false
            else if n mod (f+2) = 0
            then false
            else prime' (f+6) r
      in
        if n mod 2 = 0
        then false
        else if n mod 3 = 0
        then false
        else prime' 5 (intSqrt n)
      end;
in
fun findPrimes n xs =
    if n > 2000000
    then xs
    else if isPrime n
    then findPrimes (n+2) (n::xs)
    else findPrimes (n+2) xs
end;
val solution =
    List.reduce
        op +
        0
        (findPrimes 11 [7,5,3,2]);
