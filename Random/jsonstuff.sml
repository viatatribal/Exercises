(* This is a bunch of code to understand how the JSON library from
 * SML N/J works. *)

(* include the JSON library *)
CM.make "/usr/local/sml/smlnj-lib/JSON/json-lib.cm";

open JSON;
structure ju = JSONUtil;

(* Turn a list of JSON integers into an int list *)
val arr = JSON.ARRAY [JSON.INT 1, JSON.INT 5, JSON.INT 0];
ju.arrayMap ju.asInt arr;

(* Parsing file *)
val jsonfile = JSONParser.parseFile "jsonexample.json";

fun printObjectNames [] = []
  | printObjectNames ((s,j)::js) =
    s :: (printObjectNames (js));

fun stFinder file st =
    List.exists
        (fn (x,y) => case y of
                         STRING(r) => r = st
                      | _ => false )
        file;

fun stSearch file =
    let fun s' [] = []
          | s' (j::js) =
            case j of
                OBJECT(w) => redSearchObject w @ s' js
              | ARRAY(w) => redSearch w @ s' js
              | STRING(w) => s' js
              | _  => ("int", j)::(s' js)
    in
      s' file
    end
and stSearchObject w =
    if stFinder w "x"
    then []
    else
      List.concat
      (map (fn (x,y) => redSearch [y]) w);
