(* --- Part Two ---
 *
 * You just finish implementing your winning light pattern when you
 * realize you mistranslated Santa's message from Ancient Nordic
 * Elvish.
 *
 * The light grid you bought actually has individual brightness
 * controls; each light can have a brightness of zero or more.
 * The lights all start at zero.
 *
 * The phrase turn on actually means that you should increase the
 * brightness of those lights by 1.
 *
 * The phrase turn off actually means that you should decrease the
 * brightness of those lights by 1, to a minimum of zero.
 *
 * The phrase toggle actually means that you should increase the
 * brightness of those lights by 2.
 *
 * What is the total brightness of all lights combined after
 * following Santa's instructions?
 *
 * For example:
 *
 *   turn on 0,0 through 0,0 would increase the total brightness by 1.
 *   toggle 0,0 through 999,999 would increase the total brightness
 * by 2000000.
 *)

local
  datatype Operations = ON | OFF | TOGGLE;
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

  fun removeWord words =
      map
          (fn w =>
              (List.filter
                   (fn s => not (s = "through")
                            andalso not (s = "turn"))) w)
          (map (String.tokens Char.isSpace) words);

  fun getNumbers xs =
      let
        val [x1,y1,x2,y2] =
            map (valOf o Int.fromString)
                (List.concat
                     (map (String.tokens Char.isPunct) xs))
      in
        (x1,y1,x2,y2)
      end;

  (* get all values between (x1,y1) and (x2,y2) include *)
  fun gridX x y1 y2 =
      if y1 > y2
      then []
      else (x,y1)::(gridX x (y1+1) y2);
  fun gridNumbers (x1,y1,x2,y2) =
      if x1 > x2
      then []
      else (gridX x1 y1 y2)::(gridNumbers (x1+1,y1,x2,y2));


  val l = Array.tabulate (1000*1000, fn _ => 0);

  fun turnOn xs =
      map
          (fn (x,y) =>
              let
                val light = Array.sub(l, x*1000+y)
              in
                (Array.update (l, (x*1000+y), light+1))
              end)
          xs;
    fun turnOff xs =
      map
          (fn (x,y) =>
              let
                val light = Array.sub(l, x*1000+y)
                val extra = if light = 0 then 0 else ~1
              in
                (Array.update (l, (x*1000+y), light + extra))
              end)
          xs;
  fun toggle xs =
      map
          (fn (x,y) =>
              let
                val light = Array.sub(l, x*1000+y)
              in
                (Array.update (l, (x*1000+y), light + 2))
              end)
          xs;

  (* convert from ["on/off/toggle", "x1,y1", "x2,y2"]
   * to (ON/OFF/TOGGLE, ["x1,y1","x2,y2"]) *)
  fun actionToOperations [] = []
    | actionToOperations (x::xs) =
      case (hd x) of
           "on"     => (ON, (tl x))::(actionToOperations xs)
         | "off"    => (OFF, (tl x))::(actionToOperations xs)
         | "toggle" => (TOGGLE, (tl x))::(actionToOperations xs);

  (* Apply each of the operations (ON/OFF/TOGGLE)
   * to ligths in the rectangle (x1,y1)x(x2,y2) *)
  fun applyOperations [] = ()
    | applyOperations ((ope,x)::xs) =
      let
        val grid = List.concat (gridNumbers (getNumbers x))
      in
        case ope of
            ON     => (turnOn   grid; applyOperations xs)
          | OFF    => (turnOff  grid; applyOperations xs)
          | TOGGLE => (toggle   grid; applyOperations xs)
      end;

in
fun decoration actions =
    let
      val _ = (applyOperations
                   ((actionToOperations o removeWord o
                     convert o getContent) actions))
      val l = Array.toList l
    in
      List.reduce
          (op +)
          0
          l
    end
end;
val solution = decoration "day6input.txt";
