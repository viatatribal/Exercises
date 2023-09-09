(* 2520 is the smallest number that can be divided by
 * each of the numbers from 1 to 10 without any remainder.
 * What is the smallest positive number that is evenly
 * divisible by all of the numbers from 1 to 20?
 *)

local
  (* All primes below 20 *)
  val primes = [2,3,5,7,11,13,17,19];
  (* For any prime below 20, we get the biggest power of it that
   * is still under 20 *)
  fun biggestPower n m =
      let
        val n' = n*m
      in
        if n' > 20
        then n
        else biggestPower n' m
      end;
in
  (* Generate the powers of any prime below 20 that is
   * still below 20 *)
  fun powerGen [] xs = xs
    | powerGen (p::ps) xs =
      powerGen ps ((biggestPower p p)::xs);
end;

val solution =
    List.reduce
        (fn (x,y) => x*y)
        1
        (powerGen primes []);
