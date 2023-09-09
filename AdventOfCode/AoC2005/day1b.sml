(* --- Part Two ---
 *
 * Now, given the same instructions, find the position of the first
 * character that causes him to enter the basement (floor -1).
 * The first character in the instructions has position 1,
 * the second character has position 2, and so on.
 *
 * For example:
 *
 *   ) causes him to enter the basement at character position 1.
 *   ()()) causes him to enter the basement at character position 5.
 *
 * What is the position of the character that causes Santa
 * to first enter the basement?
 *)


local
  (* Get all the contents from the txt file *)
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  (* Convert from TextIO.vector to char list *)
  fun convert file =
      (String.explode o String.concat o
       (String.tokens Char.isCntrl)) file;
  (* Get the right position *)
  fun count  n pos (x::xs) =
      if n = ~1
      then pos-1
      else
        case x of
            #"(" => count (n+1) (pos+1) xs
          | #")" => count (n-1) (pos+1) xs;
in
fun santa file =
    let
      val floor = (convert o getContent) file
    in
      count 0 1 floor
    end
end;
