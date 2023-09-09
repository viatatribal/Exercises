(* --- Part Two ---
 *
 * The elves are also running low on ribbon. Ribbon is all the
 * same width, so they only have to worry about the length they
 * need to order, which they would again like to be exact.
 *
 * The ribbon required to wrap a present is the shortest distance
 * around its sides, or the smallest perimeter of any one face. Each
 * present also requires a bow made out of ribbon as well; the feet
 * of ribbon required for the perfect bow is equal to the cubic feet
 * of volume of the present. Don't ask how they tie the bow, though;
 * they'll never tell.
 *
 * For example:
 *
 *   A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of
 * ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the
 * bow, for a total of 34 feet.
 *   A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of
 * ribbon to wrap the present plus 1*1*10 = 10 feet of ribbon for
 * the bow, for a total of 14 feet.
 *
 * How many total feet of ribbon should they order?
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
  (* Get the minimum of all faces in a triple and their produdct *)
  fun products (a::b::[c]) =
    min(2*a+2*b, min(2*a+2*c, 2*c+2*b)) + a*b*c;
in
fun present file =
    List.reduce
        (fn (x,y) => x + y)
        0
        (List.map
             (fn n => products n)
             ((getNumbers o getContent) file))
end;
