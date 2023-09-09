(* Starting in the top left corner of a 2 x 2 grid,
 * and only being able to move to the right and down,
 * there are exactly 6 routes to the bottom right corner.
 *
 * How many such routes are there through a 20 x 20 grid?
 *)

structure ht  = HashTable;

val tbl : (string, IntInf.int) ht.hash_table = ht.mkTable
          (HashString.hashString, op=) (350, Empty);

local
  (* HashTable operations *)
  fun toSt h =
      let
        val (x,y) = h
      in
        (Int.toString x) ^ " " ^ (Int.toString y)
      end;
  fun insert l h =
      ht.insert tbl (toSt l, h);
  fun lookup l =
      ht.lookup tbl (toSt l);
  fun hasValue l =
      ht.inDomain tbl (toSt l);
in
(* The value at position (x,y) [x != 0 and y != 0,
 * for those the value is 1] is
 * (left value [x-1,y] + up value [x,y-1]).
 * So we create a table starting from position (0,0)
 * and walk from left to right, up to down and insert in
 * the table the values so far calculated and use them
 * to calculate new values *)
fun walkGrid (x,y) max =
    if x > max
    then walkGrid (0,y+1) max
    else if y > max
    then ()
    else if y = 0
    then ( insert (x,y) 1
         ; walkGrid (x+1, y) max)
      else if x = 0
    then  ( insert (x,y) 1
          ; walkGrid (x+1, y) max)
    else
      let
        val c1 = lookup (x-1,y)
        val c2 = lookup (x,y-1)
      in
          ( insert (x,y) (c1+c2)
          ; walkGrid (x+1, y) max)
      end
end;
val solution =
    let
      val _ = walkGrid (0,0) 20
    in
      lookup (20,20)
    end;
