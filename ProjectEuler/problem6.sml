(* The sum of the squares of the first ten natural numbers is,
 * 1² + 2² + ... + 10² = 385.
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)² = 55² = 3025
 * Hence the difference between the sum of the squares of the
 * first ten natural numbers and the square of the sum is
 * 3025 - 385 = 2640
 *
 * Find the difference between the sum of the squares of the
 * first one hundred natural numbers and the square of the sum.
 *)


fun difference n =
    n * (n+1) * (6*n*n + 6*n - 8*n - 4) div 24;

val solution = difference 100;
