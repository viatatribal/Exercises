(* --- Part Two ---
 *
 * Now find one that starts with six zeroes.
 *)

use "../../../SMLibrary/md5.sml";

local
  fun look i =
      let
        val hex =  "bgvyzdsv" ^ (Int.toString i)
        val md5 = MD5.md5 hex
      in
        if substring (md5, 0, 6) = "000000"
        then hex
        else look (i+1)
      end
in
val solution = look 1
end;
