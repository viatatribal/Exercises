(* --- Part Two ---
*
* Now, let's go the other way. In addition to finding the
* number of characters of code,
* you should now encode each code representation as a new string
* and find the number of characters of the new encoded
* representation, including the surrounding double quotes.
*
* For example:
*
*    "" encodes to "\"\"", an increase from 2 characters to 6.
*    "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
*    "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase
* from 10 characters to 16.
*    "\x27" encodes to "\"\\x27\"", an increase from 6
* characters to 11.
*
* Your task is to find the total number of characters to
* represent the newly encoded strings minus the number of
* characters of code in each original string literal.
* For example, for the strings above, the total encoded
* length (6 + 9 + 16 + 11 = 42) minus the characters in
* the original code representation (23, just like in the
* first part of this puzzle) is 42 - 23 = 19.
*)

local
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  fun convert file =
      String.tokens Char.isCntrl file;

  (* Sum of the of characters in all strings *)
  fun charSize st =
      List.foldl
          (fn (x,y) => size x + y)
          0
          st;
  (* Sum of the of characters in all new strings *)
  fun newStringSize st =
      foldl
          (fn (x,y) => size x + y + 2)
          0
          (map (fn c => String.toCString c) st);
in
val solution = let
  val words = (convert o getContent) "day8input.txt"
  val charsum = charSize words
  val newcharsum = newStringSize words
in
  newcharsum - charsum
end
end;
