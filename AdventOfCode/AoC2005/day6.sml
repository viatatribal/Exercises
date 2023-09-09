(* --- Day 6: Probably a Fire Hazard ---
 *
 * Because your neighbors keep defeating you in the holiday house
 * decorating contest year after year, you've decided to deploy one
 * million lights in a 1000x1000 grid.
 *
 * Furthermore, because you've been especially nice this year,
 * Santa has mailed you instructions on how to display the ideal
 * lighting configuration.
 *
 * Lights in your grid are numbered from 0 to 999 in each direction;
 * the lights at each corner are at 0,0, 0,999, 999,999, and 999,0.
 * The instructions include whether to turn on, turn off, or toggle
 * various inclusive ranges given as coordinate pairs. Each coordinate
 * pair represents opposite corners of a rectangle, inclusive; a
 * coordinate pair like 0,0 through 2,2 therefore refers to 9 lights
 * in a 3x3 square. The lights all start turned off.
 *
 * To defeat your neighbors this year, all you have to do is set up
 * your lights by doing the instructions Santa sent you in order.
 *
 * For example:
 *
 *   turn on 0,0 through 999,999 would turn on (or leave on) every
 * light.
 *   toggle 0,0 through 999,0 would toggle the first line of 1000
 * lights, turning off the ones that were on, and turning on the
 * ones that were off.
 *   turn off 499,499 through 500,500 would turn off (or leave off)
 * the middle four lights.
 *
 * After following the instructions, how many lights are lit?
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


  val l = Array.tabulate (1000*1000, fn _ => false);

  fun turnOn xs =
      map
          (fn (x,y) => Array.update (l, x*1000+y, true))
          xs;

  fun turnOff xs  =
      map
          (fn (x,y) => Array.update (l, x*1000+y, false))
          xs;

  fun toggle xs =
      map
          (fn (x,y) =>
              let
                val light = Array.sub(l, x*1000+y)
              in
                (Array.update (l, (x*1000+y), not light))
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
      length
          (List.filter
               (fn n => n)
               l)
    end
end;
val solution = decoration "day6input.txt";
