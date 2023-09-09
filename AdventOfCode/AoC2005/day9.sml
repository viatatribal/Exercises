(* --- Day 9: All in a Single Night ---
 *
 * Every year, Santa manages to deliver all of his presents
 * in a single night.
 *
 * This year, however, he has some new locations to visit; his elves
 * have provided him the distances between every pair of locations.
 * He can start and end at any two (different) locations he wants,
 * but he must visit each location exactly once. What is the shortest
 * distance he can travel to achieve this?
 *
 * For example, given the following distances:
 *
 * London to Dublin = 464
 * London to Belfast = 518
 * Dublin to Belfast = 141
 *
 * The possible routes are therefore:
 *
 * Dublin -> London -> Belfast = 982
 * London -> Dublin -> Belfast = 605
 * London -> Belfast -> Dublin = 659
 * Dublin -> Belfast -> London = 659
 * Belfast -> Dublin -> London = 605
 * Belfast -> London -> Dublin = 982
 *
 * The shortest of these is London -> Dublin -> Belfast = 605,
 * and so the answer is 605 in this example.
 *
 * What is the distance of the shortest route?
 *)
structure ht  = HashTable;

val graph : (string, (string * int) list) ht.hash_table = ht.mkTable
              (HashString.hashString, op=) (350, Empty);
local
(* HashTable operations *)
  fun insert l h =
      ht.insert graph (l, h);
  fun lookup l =
      ht.lookup graph l;
  fun hasValue l =
      ht.inDomain graph l;

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
                   (fn s => not (s = "to")
                            andalso not (s = "=")) w))
          (map (String.tokens Char.isSpace) words);
  fun s2i s =
      (valOf o Int.fromString) s;

  (* For a given town, we create a list of all its neighbours
   * and their distance *)
  fun addEdge from (to,cost) edges =
      if List.exists (fn (t,c) => t = to) edges
      then edges
      else (to,s2i cost)::edges;
  fun makeEdges from  []      edges = edges
    | makeEdges from (to::ts) edges =
      let
        val [city1,city2,cost] = to
      in
        if from = city1 then
          makeEdges from ts (addEdge from (city2,cost) edges)
        else if from = city2 then
          makeEdges from ts (addEdge from (city1,cost) edges)
        else
          makeEdges from ts edges
      end;
  (* Get all towns from the input *)
  fun getAllTowns   []    towns = towns
    | getAllTowns (t::ts) towns =
      let
        val [city1, city2, cost] = t
      in
        getAllTowns ts (city1::city2::towns)
      end;

  (* Create a graph connecting all towns *)
  fun makeGraph   []    locations = ()
    | makeGraph (t::ts) locations =
      if hasValue t
      then makeGraph ts locations
      else
        let
          val connections = makeEdges t locations []
        in
           ( insert t connections
           ; makeGraph ts locations )
        end;

  (* To create circuits *)
  fun isVisited city visited =
      List.exists (fn c => c = city) visited;

  (* Brute-force all hamilton circuits starting
   * from the same town (we end up getting unvalid ones,
   * sadly)*)
  fun hamilton ([], cost, visited) = [(Int.toString cost)::visited]
    | hamilton ((t::ts), cost, visited) =
      let
        val (city, c) = t
      in
        if isVisited city visited
        then
          hamilton (ts, cost, visited)
        else
          hamilton (lookup city, cost+c, (city::visited))
          @
          hamilton (ts, cost, visited)
      end;

  (* Since we have 8 towns, all valid hamilton circuits
   * must have 9 elements (circuit + cost)
   * Sadly, we still end up having repeated circuits *)
  fun removeUselessCircuists circuit =
      List.filter (fn n => length n = 9) circuit;
  fun makeAllHamilton [] visited circuit = circuit
    | makeAllHamilton (t::ts) visited circuit =
      if isVisited t visited
      then makeAllHamilton ts visited circuit
      else
        let
          val c = hamilton (lookup t, 0, [t])
        in
          makeAllHamilton ts (t::visited) (c @ circuit)
        end;

  fun getMinimun circuits =
      List.reduce
          (fn (x,y) => Int.min(x, y))
          0
          (map (fn x => (valOf o Int.fromString o hd) x) circuits);
in
val solution =
    let
      val locations = (removeWord o convert o getContent) "day9input.txt"
      val _ = makeGraph (getAllTowns locations []) locations
    in (getMinimun o removeUselessCircuists) (makeAllHamilton (getAllTowns locations []) [] [])
    end
end;
