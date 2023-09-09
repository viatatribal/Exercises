(*--- Part Two ---
 *
 * Neat, right? You might also enjoy hearing John Conway talking
 * about this sequence (that's Conway of Conway's Game of Life fame).
 *
 * Now, starting again with the digits in your puzzle input,
 * apply this process 50 times. What is the length of the new result?
 *)
local
fun i2c n =
    (valOf o Char.fromString o Int.toString) n;

fun lookAndSay c   []    num = i2c num :: c :: []
  | lookAndSay c (x::xs) num =
    if c = x
    then lookAndSay c xs (num+1)
    else (i2c num) :: c :: (lookAndSay x xs 1);
in
fun loop num i =
    if i = 50
    then num
    else loop (lookAndSay (hd num) (tl num) 1) (i+1)
end;
val solution = length (loop (explode "3113322113") 0);
