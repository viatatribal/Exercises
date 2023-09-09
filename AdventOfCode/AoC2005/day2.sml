(* --- Day 2: I Was Told There Would Be No Math ---
 *
 * The elves are running low on wrapping paper, and so they need
 * to submit an order for more. They have a list of the dimensions
 * (length l, width w, and height h) of each present, and only want
 * to order exactly as much as they need.
 *
 * Fortunately, every present is a box (a perfect right rectangular
 * prism), which makes calculating the required wrapping paper for
 * each gift a little easier: find the surface area of the box,
 * which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little
 * extra paper for each present: the area of the smallest side.
 *
 * For example:
 *
 *   A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52
 * square feet of wrapping paper plus 6 square feet of slack, for
 * a total of 58 square feet.
 *   A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10
 * = 42 square feet of wrapping paper plus 1 square foot of slack,
 * for a total of 43 square feet.
 *
 * All numbers in the elves' list are in feet. How many total
 * square feet of wrapping paper should they order?
 *)

local
  (* Get all the contents from the txt file *)
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  fun convertToListString word =
      List.map
          (fn n => String.tokens Char.isAlpha n)
                                 (String.tokens Char.isCntrl word);
  fun convertToListOptNumbers word =
      List.map
          (fn n => List.map fromString n)
          word;
  fun convertToListNumbers word =
      List.map
          (fn n =>
              List.map (fn x => case x of SOME(y) => y) n)
          word;
  fun getNumbers word =
      (convertToListNumbers o
      convertToListOptNumbers o
      convertToListString) word;
  (* Get all products of two numbers in a triple and their min
   *)
  fun products (a::b::[c]) = (
    2*a*b + 2*a*c + 2*b*c + min(a*b, min(a*c,b*c)));
in
fun present file =
    List.reduce
        (fn (x,y) => x + y)
        0
        (List.map
             (fn n => products n)
             ((getNumbers o getContent) file))
end;
