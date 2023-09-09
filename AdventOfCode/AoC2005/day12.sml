(* --- Day 12: JSAbacusFramework.io ---
 *
 * Santa's Accounting-Elves need help balancing the books after a
 * recent order. Unfortunately, their accounting software uses a
 * peculiar storage format. That's where you come in.
 *
 * They have a JSON document which contains a variety of things:
 * arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings.
 * Your first job is to simply find all of the numbers throughout
 * the document and add them together.
 *
 * For example:
 *
 *   [1,2,3] and {"a":2,"b":4} both have a sum of 6.
 *   [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
 *   {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
 *   [] and {} both have a sum of 0.
 *
 * You will not encounter any strings containing numbers.
 *
 * What is the sum of all numbers in the document?
 *)
local
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  (* Convert from TextIO.vector to char list while
   * removing all the things that can get in the way of
   * finding a number *)
  fun convert file =
      String.tokens
          (fn c => c = #":" orelse c = #","
                   orelse c = #"\"" orelse c = #"["
                   orelse c = #"]" orelse c = #"("
                   orelse c = #")" orelse c = #"}"
                   orelse c = #"{") file;
  (* Get all the numbers out of our file *)
  fun stringInt [] = []
    | stringInt (x::xs) =
      case Int.fromString x of
          SOME(y) => y::(stringInt xs)
        | NONE => stringInt xs
in
val solution =
    let
      val file = (stringInt  o convert o getContent) "day12input.json"
    in
      List.reduce op+ 0 file
    end
end;


(* include the JSON library *)
CM.make "/usr/local/sml/smlnj-lib/JSON/json-lib.cm";

open JSON;
structure ju = JSONUtil;

val jsonfile = JSONParser.parseFile "day12input.json";

local
  (* find all int in the json file *)
  fun intSearch file =
      let fun s' [] = []
            | s' (j::js) =
              case j of
                  OBJECT(w) => intSearchObject w @ s' js
                | ARRAY(w) => intSearch w @ s' js
                | STRING(w) => s' js
                | _  => ("int", j)::(s' js)
      in
        s' file
      end
  (* apply the intSearch to every value in an js Object *)
  and intSearchObject w =
      List.concat
          (map (fn (x,y) => intSearch [y]) w);
in
val solution =
    List.reduce
        (fn (x,y) => x+y)
        0
        (map (fn (x,y) => ju.asInt y) (intSearch [jsonfile]))
end;
