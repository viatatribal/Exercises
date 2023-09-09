(*--- Part Two ---
 * The next year, to speed up the process, Santa creates a robot
 * version of himself, Robo-Santa, to deliver presents with him.
 *
 * Santa and Robo-Santa start at the same location (delivering two
 * presents to the same starting house), then take turns moving
 * based on instructions from the elf, who is eggnoggedly
 * reading from the same script as the previous year.
 *
 * This year, how many houses receive at least one present?
 *
 * For example:
 *
 *   ^v delivers presents to 3 houses, because Santa goes north,
 * and then Robo-Santa goes south.
 *   ^>v< now delivers presents to 3 houses, and Santa and
 * Robo-Santa end up back where they started.
 *   ^v^v^v^v^v now delivers presents to 11 houses,
 * with Santa going one direction and Robo-Santa going the other.
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

  fun giftRobot []     posS posR h = h
    | giftRobot (d::ds) posS posR h =
      let
        val posR' = directionToPlace d posR
        val h' = updateTable h (placeToPos posR')
      in
        giftSanta ds posS posR' h'
      end
  and giftSanta []      posS posR h = h
    | giftSanta (d::ds) posS posR h =
      let
        val posS' = directionToPlace d posS
        val h'   = updateTable h (placeToPos posS')
      in
        giftRobot ds posS' posR h'
      end;

  val houses = ([(0,1)], 1);
in
fun gifts file =
    let
      val directions = convert (getContent file)
    in
      giftSanta directions (0,0) (0,0) houses
    end
end;

