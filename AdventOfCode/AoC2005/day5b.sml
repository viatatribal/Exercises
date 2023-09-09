(* --- Part Two ---
 *
 * Realizing the error of his ways, Santa has switched to a better
 * model of determining whether a string is naughty or nice. None
 * of the old rules apply, as they are all clearly ridiculous.
 *
 * Now, a nice string is one with all of the following properties:
 *
 *   It contains a pair of any two letters that appears at least
 * twice in the string without overlapping, like xyxy (xy) or
 * aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
 *   It contains at least one letter which repeats with exactly one
 * letter between them, like xyx, abcdefeghi (efe), or even aaa.
 *
 * For example:
 *
 *    qjhvhtzxzqqjkmpb is nice because is has a pair that appears
 * twice (qj) and a letter that repeats with exactly one letter
 * between them (zxz).
 *   xxyxx is nice because it has a pair that appears twice and a
 * letter that repeats with one between, even though the letters
 * used by each rule overlap.
 *    uurcxstgmygtbstg is naughty because it has a pair (tg) but
 * no repeat with a single letter between them.
 *    ieodomkazucvgmuy is naughty because it has a repeating letter
 * with one between (odo), but no pair that appears twice.
 *
 * How many strings are nice under these new rules?
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
     String.tokens Char.isCntrl file;

  val tripleLetters =
      ["aaa", "bbb", "ccc", "ddd", "eee", "fff", "ggg",
       "hhh", "iii", "jjj", "kkk", "lll", "mmm", "nnn",
       "ooo", "ppp", "qqq", "rrr", "sss", "ttt", "uuu",
       "vvv", "xxx", "www", "yyy", "zzz"];
  fun filterTriple w =
      List.exists
          (fn n => String.isSubstring n w)
          tripleLetters;
  (* Remove all words that have triple letters or more *)
  fun noTripleLetters words =
      List.filter
          (fn w => not (filterTriple w))
          words;

  fun removeLast xs = rev (tl (rev xs));
  val alpha = explode "abcdefghijlkmnopqrstuvwxyz";
  fun filterAltLetters w =
      List.concat
          (List.map
               removeLast
               (List.filter
               (fn n => (length n) > 1 andalso not (n = nil))
               (List.map
                    (fn n => tl (String.fields (fn c => c = n) w))
                    alpha)));
  (* Remove all words that don't have an alternating letter *)
  fun noAltWords words =
      List.filter
          (fn c =>
              List.exists (fn n => (size n) = 1)
                          (filterAltLetters c))
          words;
  (* Concatenate two following letters:
   * ['a','b','c'] -> ["ab","bc"]*)
  fun pairWord [x,y] = (implode [x,y])::[]
    | pairWord (x::y::xs) = (implode [x,y])::(pairWord (y::xs));
  fun repeatedPair    []   = false
    | repeatedPair   [x]   = false
    | repeatedPair (x::xs) =
      List.exists (fn n => n = x) xs
      orelse repeatedPair xs;
  val wToPairWord =
      (pairWord o explode);
  (* Remove all words that have no pairs *)
  fun noPairWords words =
      List.filter
          (fn n => (repeatedPair o wToPairWord) n)
          words;
in
val solution =
    (length o noPairWords o noAltWords o
     noTripleLetters o convert o getContent)
        "day5input.txt"
end;
