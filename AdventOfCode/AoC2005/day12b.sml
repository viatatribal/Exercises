(* --- Part Two ---
 *
 * Uh oh - the Accounting-Elves have realized that they
 * double-counted everything red.
 *
 * Ignore any object (and all of its children) which has any property
 *  with the value "red". Do this only for objects ({...}),
 * not arrays ([...]).
 *
 *   [1,2,3] still has a sum of 6.
 *   [1,{"c":"red","b":2},3] now has a sum of 4, because the
 * middle object is ignored.
 *   {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because
 * the entire structure is ignored.
 *   [1,"red",5] has a sum of 6, because "red" in an array has no
 * effect.
 *)

(* include the JSON library *)
CM.make "/usr/local/sml/smlnj-lib/JSON/json-lib.cm";
open JSON;
structure ju = JSONUtil;

(* Parsing file *)
val jsonfile = JSONParser.parseFile "jsonproblem.json";

local
  (* check if a value of "red" is in this js Object *)
  fun redFinder file =
      List.exists
          (fn (x,y) => case y of
                           STRING(r) => r = "red"
                         | _ => false )
          file;
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
  (* apply the intSearch to every value in an js Object
   * that doesn't have the value "red". Those that have
   * it, we remove from our list of ints *)
  and intSearchObject w =
      if redFinder w
      then []
      else
        List.concat
            (map (fn (x,y) => intSearch [y]) w);
in
val solution =
    List.reduce
        (fn (x,y) => x+y)
        0
        (map (fn (x,y) => ju.asInt y) (intSearch [jsonfile]))
end;
