(* --- Day 8: Matchsticks ---
*
* Space on the sleigh is limited this year, and so
* Santa will be bringing his list as a digital copy.
* He needs to know how much space it will take up when stored.
*
* It is common in many programming languages to provide a way
* to escape special characters in strings.
* For example, C, JavaScript, Perl, Python, and even PHP handle
* special characters in very similar ways.
*
* However, it is important to realize the difference between
* the number of characters in the code
* representation of the string literal and the number of characters
* in the in-memory string itself.
*
* For example:
*
*    "" is 2 characters of code (the two double quotes), but the
* string contains zero characters.
*    "abc" is 5 characters of code, but 3 characters in the
* string data.
*    "aaa\"aaa" is 10 characters of code, but the string itself
* contains six "a" characters and a single,
* escaped quote character, for a total of 7 characters in the
* string data.
*    "\x27" is 6 characters of code, but the string itself contains
* just one - an apostrophe ('),
* escaped using hexadecimal notation.
*
* Santa's list is a file that contains many double-quoted
* string literals, one on each line.
* The only escape sequences used are \\ (which represents a
* single backslash), \" (which represents a lone double-quote
* character),
* and \x plus two hexadecimal characters (which represents a
* single character with that ASCII code).
*
* Disregarding the whitespace in the file, what is the number of
* characters of code for string literals
* minus the number of characters in memory for the values of the
* strings in total for the entire file?
*
* For example, given the four strings above, the total number of
* characters of string code (2 + 5 + 10 + 6 = 23)
* minus the total number of characters in memory for string
* values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.
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

  (* Sum of the mumber of characters in all strings *)
  fun charSize st =
      List.foldl
          (fn (x,y) => size x + y)
          0
          st;

  (* Sum of the number of characters in the in-memory string *)
  fun memorySize st =
      List.foldl
          (fn (x,y) => (size x - 2) + y)
          0
          (List.map
               (fn x => case (String.fromString x) of
                            SOME(y) => y)
               st);

  fun isHex z =
      Char.contains "0123456789ABCDEFabcdef" z;

  (* We remove the sequence \xXX and replace it with
  * any random letter ('b' in this case). Since \xXX
  * count for a single letter, we need to replace it with
  * a new letter *)
  fun removeHex []         = []
    | removeHex [x]        = [x]
    | removeHex (x::y::xs) =
      if x = #"\\" andalso y = #"\\"
      then x::y::(removeHex xs)
      else if x = #"\\" andalso y = #"x"
      then
        #"b"::removeHex (tl (tl xs))
      else x::(removeHex (y::xs));

  (* Since we are going to walk over strings, it's better
   * to do it only in strings that may have a hex escape
   * sequence. *)
  fun noHexStrings    [] newSt = newSt
    | noHexStrings (w::ws) newSt =
      if String.isSubstring "\\x" w
      then
        noHexStrings ws ((implode (removeHex (explode w))::newSt))
      else noHexStrings ws (w::newSt);
in
val solution = let
  val words = (convert o getContent) "day8input.txt"
  val charsum = charSize words
  val noHexN = memorySize (noHexStrings words [])
in
  charsum - noHexN
end
end;
