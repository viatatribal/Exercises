(* Problem 1
 * If we list all natural numbers that are multiples of 3 or 5,
 * we get 3, 5, 6 and 9. The sum of these multiples is
 * 23. Find the sum of all the multiples of 3 or 5 below 1000.
 *)

(* We create a list of 0 up to, but not included, 1000
 * and remove the first element (0). Then we apply a filter
 * to it, removing any element that is not a multiple of
 * 3 or 5. Finally, we sum all the elements in the list.
 *)
val solution = List.reduce
                   (fn (x,y) => x + y)
                   0
                   (List.filter
                        (fn n => n mod 3 = 0 orelse n mod 5 = 0)
                        (List.tl (List.tabulate (1000, fn n => n))));
