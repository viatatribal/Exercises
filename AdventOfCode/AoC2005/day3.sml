(* --- Day 3: Perfectly Spherical Houses in a Vacuum ---
 *
 * Santa is delivering presents to an infinite two-dimensional
 * grid of houses.
 *
 * He begins by delivering a present to the house at his starting
 * location, and then an elf at the North Pole calls him via radio
 * and tells him where to move next. Moves are always exactly one
 * house to the north (^), south (v), east (>), or west (<).
 * After each move, he delivers another present to the house at his
 * new location.
 *
 * However, the elf back at the north pole has had a little too much
 * eggnog, and so his directions are a little off, and Santa ends up
 * visiting some houses more than once. How many houses receive
 * at least one present?
 *
 * For example:
 *
 *   > delivers presents to 2 houses: one at the starting location,
 * and one to the east.
 *   ^>v< delivers presents to 4 houses in a square,
 * including twice to the house at his starting/ending location.
 *   ^v^v^v^v^v delivers a bunch of presents to some very lucky
 * children at only 2 houses.
 *
 *)

(* Every house is given by (up, right), where
 * we search it in a list by up + 1000*right
 * and so we represent houses with a variable holding
 * ((pos, gifts) list, size), where size is how many houses
 * we have visited so far
 *)

local
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;

  fun convert file =
      (String.explode o String.concat o
       (String.tokens Char.isCntrl)) file;

  fun updateTable ([],size) k = ([(k,1)],size + 1)
    | updateTable ((x,y)::xs,size) k =
      if x = k
      then ((x,y+1)::xs,size)
      else
        let
          val (x',size') = updateTable (xs, size) k
        in
          ((x,y)::x', size')
        end;

  fun placeToPos (u,r) =
      u + 1000*r;

  fun directionToPlace place (u,r) =
      case place of
          #"^" => (u+1,r)
        | #">" => (u,r+1)
        | #"v" => (u-1,r)
        | #"<" => (u,r-1);

  fun giftHouses []      pos h = h
    | giftHouses (d::ds) pos h =
      let
        val pos' = directionToPlace d pos
        val h'   = updateTable h (placeToPos pos')
      in
        giftHouses ds pos' h'
      end;

  val houses = ([(0,1)], 1);
in
fun gifts file =
    let
      val directions = convert (getContent file)
    in
      giftHouses directions (0,0) houses
    end
end;
