(* --- Day 7: Some Assembly Required ---
 *
 * This year, Santa brought little Bobby Tables a set of wires and
 * bitwise logic gates! Unfortunately, little Bobby is a little under
 * the recommended age range, and he needs help assembling
 * the circuit.
 *
 * Each wire has an identifier (some lowercase letters) and can
 * carry a 16-bit signal (a number from 0 to 65535). A signal is
 * provided to each wire by a gate, another wire, or some specific
 * value. Each wire can only get a signal from one source, but can
 * provide its signal to multiple destinations. A gate provides no
 * signal until all of its inputs have a signal.
 *
 * The included instructions booklet describes how to connect the
 * parts together: x AND y -> z means to connect wires x and y to
 * an AND gate, and then connect its output to wire z.
 *
 * For example:
 *
 *   123 -> x means that the signal 123 is provided to wire x.
 *   x AND y -> z means that the bitwise AND of wire x and wire y
 * is provided to wire z.
 *   p LSHIFT 2 -> q means that the value from wire p is
 * left-shifted by 2 and then provided to wire q.
 *   NOT e -> f means that the bitwise complement of the value
 * from wire e is provided to wire f.
 *
 * Other possible gates include OR (bitwise OR) and RSHIFT
 * (right-shift). If, for some reason, you'd like to emulate the
 * circuit instead, almost all programming languages (for example,
 * C, JavaScript, or Python) provide operators for these gates.
 *
 * For example, here is a simple circuit:
 *
 * 123 -> x
 * 456 -> y
 * x AND y -> d
 * x OR y -> e
 * x LSHIFT 2 -> f
 * y RSHIFT 2 -> g
 * NOT x -> h
 * NOT y -> i
 *
 * After it is run, these are the signals on the wires:
 *
 * d: 72
 * e: 507
 * f: 492
 * g: 114
 * h: 65412
 * i: 65079
 * x: 123
 * y: 456
 *
 * In little Bobby's kit's instructions booklet (provided as your
 * puzzle input), what signal is ultimately provided to wire a?
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
      val msgs = (removeWord o convert o getContent) "day7input.txt"
      val _ = assembly (operations msgs) []
    in
      w16.toInt (lookupValue "a")
    end
end;
