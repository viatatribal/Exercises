(* The following iterative sequence is defined for the set of
 * positive integers:
 *
 * n -> n/2 (n is even)
 * n -> 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the
 * following sequence:
 *       13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 * It can be seen that this sequence (starting at 13 and
 * finishing at 1) contains 10 terms. Although it has not
 * been proved yet (Collatz Problem), it is thought that all
 * starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the
 * longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go
 * above one million.
 *)

val collatz = Array.tabulate (1000000, fn _ => (~1,false));
Array.update (collatz, 1, (0,true));


local
  fun getNumber n =
    if n < 1000000
    then Array.sub (collatz, Int.fromLarge n)
    else (~1, false);

  fun evenCollatz (n : IntInf.int, count, on) =
      let
        val (x,y) = getNumber (n div 2)
      in
        if y
        then
          let
            val _ = Array.update (collatz, on, (count+x, true))
          in (1, count+x, on) end
        else (n div 2, count+1, on)
      end;

  fun oddCollatz (n : IntInf.int, count, on) =
      let
        val (x,y) = getNumber (3 * n + 1)
      in
        if y
        then
          let
            val _ = Array.update (collatz, on, (count+x, true))
          in (1, count+x, on) end
        else (3 * n + 1, count+1, on)
      end;

  fun collatzProblem (n : IntInf.int, count, maxcount, bestn, on) =
      if n = 1
      then if maxcount > count
           then (maxcount, bestn)
           else (count, on)
      else if n mod 2 = 0
      then
        let
          val (n, count, on) = evenCollatz (n, count, on)
        in
          collatzProblem (n, count, maxcount, bestn, on)
        end
      else
        let
          val (n, count, on) = oddCollatz (n, count, on)
        in
          collatzProblem (n, count, maxcount, bestn, on)
        end;
in
fun loop i maxcount bestn =
    if i = 1000000
    then (maxcount, bestn)
    else
      let
        val (maxcount, bestn) =
            collatzProblem (i, 1, maxcount, bestn, Int.fromLarge i)
      in
        loop (i+1) maxcount bestn
      end
end;
val solution = loop (Int.toLarge 2) 0 1;
