(* A palindromic number reads the same both ways.
 * The largest palindrome made from the product of two 2-digit numbers
 * is 9009 = 91 x 99
 *
 * Find the largest palindrome made from the product of two 3-digit
 * numbers.
 *)


local
  (* Check if the string is equal to its reverse *)
  fun checkPalindrome n  =
      n = (String.rev n);
  (* Generate all multiples of 3-digit numbers *)
  fun gen n m xs =
      if n = 999 andalso m = 999
      then ((n*m)::xs)
      else
        if n = 1000
        then gen (m+1) (m+1) xs
        else gen (n+1) m ((n*m)::xs)
in
fun generatePalindromes n =
    gen n 100 []
end;

val solution = List.reduce
              (fn (x,y) => max(x,y))
              0
              (List.filter
                   (fn z => checkPalindrome (toString z))
                   (generatePalindromes 100));
