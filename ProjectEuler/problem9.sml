(* A Pythagorean triplet is a set of three natural numbers,
 * a < b < c, for which,
 * a² + b² = c².
 *
 * For example, 3² + 4² = 9 + 16 = 25 = 5².
 *
 * There exists exactly one Pythagorean triplet for which
 * a + b + c = 1000.
 * Find the product abc.
 *)

(* The Pythagorean triplets can be found by using the following
 * formulas:
 * a = m² - n², b = 2mn, c = m² + n².
 * a + b + c = 1000, we have
 * (m² - n²) + (2mn) + (m² + n²) = 1000
 * 2m² + 2mn = 1000
 * 2m(m+n) = 1000
 * m(m+n) = 500
 * Thus m and (m+n) divides 500 and we only need to check for the
 * divisors of 500.
 * That is, all m and n such that m * (m+n) = 500.
 * Since we know that m divides 500 we can find n by doing:
 * n = 500/m - m.
 *)

local
  val divisors = [1,2,4,5,10,20,25,50,100,125,250,500];

  (* Generate all n such that m*(m+n) = 500 *)
  fun genN m = (500 div m) - m;

  fun product a b c =
      Int.toLarge a * Int.toLarge b * Int.toLarge c;
  (* Check all possible a b c *)
  fun pythagoreanTriplets [] = 0
    | pythagoreanTriplets (m::ms) =
      let
        val n = genN m
        val a = m*m - n*n
        val b = 2*m*n
        val c = m*m + n*n
      in
        if a > 0 andalso b > 0 andalso c > 0
           andalso a+b+c = 1000
        then product a b c
        else pythagoreanTriplets ms
      end;
in
val solution = pythagoreanTriplets divisors
end;
