(* --- Day 5: Doesn't He Have Intern-Elves For This? ---
 *
 * Santa needs help figuring out which strings in his text file
 * are naughty or nice.
 *
 * A nice string is one with all of the following properties:
 *
 *   It contains at least three vowels (aeiou only), like aei,
 * xazegov, or aeiouaeiouaeiou.
 *   It contains at least one letter that appears twice in a row,
 * like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
 *   It does not contain the strings ab, cd, pq, or xy, even
 * if they are part of one of the other requirements.
 *
 * For example:
 *
 *   ugknbfddgicrmopn is nice because it has at least three vowels
 * (u...i...o...), a double letter (...dd...), and none of
 * the disallowed substrings.
 *   aaa is nice because it has at least three vowels and a double
 * letter, even though the letters used by different rules overlap.
 *   jchzalrnumimnmhp is naughty because it has no double letter.
 *   haegwjzuvuyypxyu is naughty because it contains the string xy.
 *   dvszwmarrgswjxmb is naughty because it contains only one vowel.
 *
 * How many strings are nice?
 *)
local
  (* Get all the contents from the txt file *)
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  (* Convert from TextIO.vector to string list *)
  fun convert file =
     String.tokens Char.isCntrl file;

  (* Remove all words that have ab, cb, pq or, xy *)
  fun noABCDPQXY words =
      List.filter
          (fn n =>
              not (String.isSubstring "ab" n orelse
                   String.isSubstring "cd" n orelse
                   String.isSubstring "pq" n orelse
                   String.isSubstring "xy" n))
          words;

  fun isVowel l =
      Char.contains "aeiou" l;
  val filterVowel =
      (implode o (List.filter (fn c => isVowel c)) o explode);
  (* Remove all words that have 2 or less vowel *)
  fun notEnoughVowels words =
      List.filter
          (fn w => (size (filterVowel w)) > 2)
          words;

  val doubleLetters =
      ["aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii",
       "jj", "kk", "ll", "mm", "nn", "oo", "pp", "qq", "rr",
       "ss", "tt", "uu", "vv", "xx", "ww", "yy", "zz"];
  fun filterDouble w =
      List.exists
          (fn n => String.isSubstring n w)
          doubleLetters;
  (* Remove all words that doesn't have double letters *)
  fun noDoubleLetters words =
      List.filter
          (fn w => filterDouble w)
          words;
in
val solution =
    (length o noDoubleLetters o notEnoughVowels o noABCDPQXY o
     convert o getContent) "day5input.txt"
end;
