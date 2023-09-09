(* --- Part Two ---
 *
 * Now, take the signal you got on wire a, override wire b to that
 * signal, and reset the other wires (including wire a).
 * What new signal is ultimately provided to wire a?
 *)

use "../../../SMLibrary/Word16.sml";
structure w16 = Word16;
structure ht  = HashTable;

val tbl : (string, word) ht.hash_table = ht.mkTable
              (HashString.hashString, op=) (350, Empty);

val i2w16 = w16.fromInt;

  datatype Gate  = AND | OR | NOT | LS | RS | CONNECT;
  datatype Value = INT | STRING;
local

  (* HashTable operations *)
  fun insertValueInt l h =
      ht.insert tbl (l, i2w16 h);
  fun insertValueWord l h =
      ht.insert tbl (l, h);
  fun lookupValue l =
      ht.lookup tbl l;
  fun hasValue l =
      ht.inDomain tbl l;

  (* Input-related functions *)
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;
  fun convert file =
      String.tokens Char.isCntrl file;
  fun removeWord words =
      map
          (fn w =>
              (List.filter
                   (fn s => not (s = "->")) w))
          (map (String.tokens Char.isSpace) words);
  fun letterOrNum x =
      case (Int.fromString x) of
          SOME(y) => (INT, x)
        | NONE    => (STRING, x);
  fun valueOrNum x =
      case (letterOrNum x) of
          (INT, y)    => (w16.fromInt ((valOf o Int.fromString) y),(true))
        | (STRING, y) => ( if hasValue y
                           then (lookupValue y, true)
                           else (i2w16 0, false) )
  (* Gate-related functions
   * If wire x and wire y have a signal (a value)
   * then we apply the operation, otherwise,
   * we return a false value so we add the operation
   * to the stack.*)
  fun andGate wires =
      let
        val [x,y,z] = wires
        val (x,t1) = valueOrNum x
        val (y,t2) = valueOrNum y
      in
        if t1 andalso t2
        then (insertValueWord z (w16.andb(x,y)), true)
        else ((), false)
      end;
  fun orGate wires =
      let
        val [x,y,z] = wires
        val (x,t1) = valueOrNum x
        val (y,t2) = valueOrNum y
      in
        if t1 andalso t2
        then (insertValueWord z (w16.orb(x,y)), true)
        else ((), false)
      end;
  fun lsGate wires =
      let
        val [x,y,z] = wires
        val (x,t1) = valueOrNum x
        val (y,t2) = valueOrNum y
      in
        if t1 andalso t2
        then (insertValueWord z (w16.shiftl(x,y)), true)
        else ((), false)
      end;
  fun rsGate wires =
      let
        val [x,y,z] = wires
        val (x,t1) = valueOrNum x
        val (y,t2) = valueOrNum y
      in
        if t1 andalso t2
        then (insertValueWord z (w16.shiftr(x,y)), true)
        else ((), false)
      end;
  fun notGate wires =
      let
        val [x,y] = wires
        val (x,t) = valueOrNum x
      in
        if t
        then (insertValueWord y (w16.notb(x)), true)
        else ((), false)
      end;
  fun connectGate wires =
      let
        val [x,y] = wires
        val (ty,_) = letterOrNum x
        val (x,t) = valueOrNum x
      in
        if t
        then
          case ty of
                INT => (insertValueInt y (w16.toInt x), true)
           | STRING => (insertValueWord y x, true)
        else ((), false)
      end;

  (* apply each gate to the wires *)
  fun applyAction (CONNECT, wires) = connectGate wires
    | applyAction (NOT, wires)     = notGate wires
    | applyAction (AND, wires)     = andGate wires
    | applyAction (OR, wires)      = orGate wires
    | applyAction (LS, wires)      = lsGate wires
    | applyAction (RS, wires)      = rsGate wires;

  (* functions related to converting
   * string-operation to datatype-operation *)
  fun messageToGate act =
      case (hd act) of
          "AND"    => AND
        | "OR"     => OR
        | "LSHIFT" => LS
        | "RSHIFT" => RS;
  fun messageToAction msg =
      if length msg = 2
      then (CONNECT, msg)
      else if length msg = 3
      then (NOT, tl msg)
      else
        let
          val y = messageToGate (tl msg)
          val msg = (hd msg)::(tl (tl msg))
        in
          (y, msg)
        end;
  fun operations [] = []
    | operations (m::msg) =
      (messageToAction m)::(operations msg);

  (* The main function. We keep a stack whenever
   * an operation was impossible to execute because
   * some wire had no signal (value) *)
  fun assembly [] [] = ()
    | assembly [] stack = assembly stack []
    | assembly (x::xs) stack =
      let
        val (_,t) = applyAction x
      in
        if t
        then assembly xs stack
        else assembly xs (x::stack)
      end
in
val solution =
    let
      val msgs = (removeWord o convert o getContent) "day7binput.txt"
      val _ = assembly (operations msgs) []
    in
      w16.toInt (lookupValue "a")
    end
end;
