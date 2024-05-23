(*
 * Sean Berlin
 * Program Description:
 * This program contains various OCaml functions for performing different tasks, including mathematical calculations, recursive operations, and data processing. 
  * Each function is described in it respected question.
 * ************************************************************* *)

 (* 1. Write a function harmonic that uses recursion to compute n-th harmonic number.  Read:  https://en.wikipedia.org/wiki/Harmonic_number
      For example, harmonic 2;; returns 1.5 *)
let rec harmonic : int -> float = fun n -> if n = 1 then 1.0 else 1.0 /. float_of_int n +. harmonic (n - 1);;



(* 2. Write a function howManyAboveAverage that returns how many of three integer inputs are above its average value.  
      For example, howManyAboveAverage 1 3 3;; returns 2 *)
let howManyAboveAverage : int -> int -> int -> int = fun x y z -> let average = (x + y + z) / 3 in let count_above_average = (if x > average then 1 else 0) + (if y > average then 1 else 0) + (if z > average then 1 else 0) in count_above_average;;



(* 3. Write a function howManyWithinThreshold that returns how many of the first three arguments are within the threshold (the fourth argument) of the average of the first three arguments. 
       For example, howManyWithinThreshold 10 1 2 3.5;; returns 2 *)
let howManyWithinThreshold : int -> int -> int -> float -> int = fun x y z t -> let average = float_of_int (x + y + z) /. 3.0 in let count_within_threshold = (if abs_float (float_of_int x -. average) <= t then 1 else 0) + (if abs_float (float_of_int y -. average) <= t then 1 else 0) + (if abs_float (float_of_int z -. average) <= t then 1 else 0) in count_within_threshold;;



(* 4. Write a function mandn that uses recursion to compute m + (m+1) + (m+2) + ... + (m + (n-1)) + (m + n).  
      For example, mandn 1 2;; returns 6 *)	  
let rec mandn : int -> int -> int = fun m n -> if n = 0 then m else m + mandn (m + 1) (n - 1);;



(* 5. Write a function sum that uses recursion to compute the sum of all numbers from 1 to n, where n is greater than or equal to 1.
      For example, sum 3;; returns 6 *)
let rec sum : int -> int = fun n -> if n <= 0 then 0 else n + sum (n - 1);;



(* 6. Write a function getBinary that uses recursion to convert an integer n (where n is greater than or equal to 0) to its binary representation.
      For example, getBinary 12;; returns 1100 
                   getBinary 7;;  returns 111
                   getBinary 42;; returns 101010 
      Hint:    if n's binary representation is 10010101011
            (n / 2)'s binary representation is 1001010101
            (N % 2)'s binary representation is           1 *)	  
let rec getBinary : int -> int = fun n -> if n = 0 then 0 else (n mod 2) + 10 * getBinary (n / 2);;



(* 7. Write a function howManyDigits that uses recursion to count the digits of an integer n (where n is greater than or equal to 1).
      For example, howManyDigits 978;; returns 3 *)
let rec howManyDigits : int -> int = fun n -> if n < 10 then 1 else 1 + howManyDigits (n / 10);;



(* 8. Write a function orderTriple that takes a triple, and returns a version in increasing order.
      For example, orderTriple (2, 1, 3);; returns (1, 2, 3) *)
let orderTriple : int * int * int -> int * int * int = fun (x, y, z) -> if x <= y && y <= z then (x, y, z) else if x <= z && z <= y then (x, z, y) else if y <= x && x <= z then (y, x, z) else if y <= z && z <= x then (y, z, x) else if z <= x && x <= y then (z, x, y) else (z, y, x);;
