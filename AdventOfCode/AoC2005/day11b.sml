(*--- Part Two ---
 *
 * Santa's password expired again. What's the next one?
 *)
local
fun increaseLetter c =
    (chr (ord c + 1));

fun wrapAround [] = []
  | wrapAround (c::cs) =
    if c = #"z"
    then #"a" :: (wrapAround cs)
    else (increaseLetter c)::cs;

fun forceMove [] = []
  | forceMove (x::xs) =
    #"a" :: (forceMove xs);

fun invalidLetter [] = []
  | invalidLetter (c::cs) =
    if c = #"i" orelse c = #"o" orelse c = #"l"
    then
     (increaseLetter c) :: (forceMove cs)
    else
      c :: (invalidLetter cs);

fun hasTwoDoubleLetters word =
     List.foldl
        (fn (x,y) => if String.isSubstring x word
                     then 1+y
                     else y)
        0
        ["aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "jj",
         "kk", "mm", "nn", "pp", "qq", "rr", "ss", "tt", "uu",
         "vv", "ww", "xx", "yy", "zz"];

fun hasIncreasingTriple (a::b::c::xs) =
    let
      val a' = ord a
      val b' = ord b
      val c' = ord c
    in
      if (a'+1) = b' andalso (b'+1) = c'
      then true
      else hasIncreasingTriple (b::c::xs)
    end
  | hasIncreasingTriple _ = false;

fun updateWord w =
    let
      val w' = rev w
    in
      rev (wrapAround w')
    end;
in
fun nextPassword c =
    let
      val pw = invalidLetter c
      val pw' = implode pw
    in
      if hasIncreasingTriple pw andalso hasTwoDoubleLetters pw' > 1
      then pw'
      else nextPassword (updateWord pw)
    end
end;
val solution = nextPassword (updateWord (explode "vzbxxyzz"));

