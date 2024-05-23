(*
 * Sean Berlin
 * Program Description:
 * This program contains various OCaml functions for performing different tasks, including boolean operations, mathematical calculations, and conditional checks. 
  * Each function is described in it respected question:
 * ************************************************************* *)

(* 1. Write function "opp" that returns the boolean complement (opposite) of its argument.  
      For example, opp (1 = 2);; returns true *)	  
let opp : bool -> bool = fun x -> not x;;



(* 2. Write function "dividesEvenlyByThree" that returns whether some dividend is evenly divisible by the divisor 3.
      For example, dividesEvenlyByThree 6;; returns true *)
let dividesEvenlyByThree : int -> bool = fun x -> x mod 3 = 0;;



(* 3. Write function "third" that returns the third greatest of four given arguments.
      For example, third 1 2 3 3;; returns 2 *)	  
let third : 'a -> 'a -> 'a -> 'a -> 'a = fun w x y z -> let max_of_two a b = if a >= b then a else b in let max_of_three a b c = max_of_two a (max_of_two b c) in if w >= x && w >= y && w >= z then if x >= y && x >= z then max_of_two y z else if y >= z then max_of_two x z else y else if x >= y && x >= z then if w >= y && w >= z then max_of_two y z else if y >= z then max_of_two w z else y else if y >= z then max_of_two w x else z;;



(* 4. Write function "nor" that computes the NOR gate of two boolean arguments.
      The NOR gate is a combination OR gate followed by an inverter. 
      Its output is "true" if both inputs are "false."  Otherwise, the output is "false." 
      For example, nor (1=2) (2=3);; returns true *)
let nor : bool -> bool -> bool = fun x y -> not (x || y);;



(* 5. Write function "squares" that takes three integer numbers as arguments, 
      and returns the sum of the squares of the two large numbers.  
      For example, squares 1 2 3;; returns 13. *)
let squares : int -> int -> int -> int = fun x y z -> let largest_two a b c = if a <= b && a <= c then (b, c) else if b >= c then (a, b) else (a, c) in let (largest, second_largest) = largest_two x y z in (largest * largest) + (second_largest * second_largest);;



(* 6. Write function "ceilingDecimal" that calculates the ceiling of a float, but returns it as an int rather than a float. 
      If you are not familiar with the ceiling funtion, read: https://www.mathsisfun.com/sets/function-floor-ceiling.html 
      For example, ceilingDecimal 15.1;; returns 16 *)
let ceilingDecimal : float -> int = fun x -> let int_x = int_of_float x in if x > float_of_int int_x then int_x + 1 else int_x;;



(* 7. Write function "letterGrade" that returns the equivalent letter grade for a given numerical integer grade below.

		Letter Grade	Numerical Grade x
		A				93 <= x <= 100
		A-				90 <= x < 93
		B+				87 <= x < 90
		B				83 <= x < 87
		B-				80 <= x < 83
		C+				77 <= x < 80
		C				73 <= x < 77
		C-				70 <= x < 73
		D+				67 <= x < 70
		D				63 <= x < 67
		D-				60 <= x < 63
		F				< 60

      For example, letterGrade 91;; returns A- *)
let letterGrade : int -> string = fun x -> if x >= 93 && x <= 100 then "A" else if x >= 90 && x < 93 then "A-" else if x >= 87 && x < 90 then "B+" else if x >= 83 && x < 87 then "B" else if x >= 80 && x < 83 then "B-" else if x >= 77 && x < 80 then "C+" else if x >= 73 && x < 77 then "C" else if x >= 70 && x < 73 then "C-" else if x >= 67 && x < 70 then "D+" else if x >= 63 && x < 67 then "D" else if x >= 60 && x < 63 then "D-" else "F";;



(* 8. Write function "findDays" that returns the number of days in a month. 
      The function takes in as input an integer representing a month, and outputs the
      number of days.  You can assume that February has 28 days.
      For example, findDays 12;; returns 31
  
      The function returns -1 for any error cases.
      For example, findDays 13;; returns -1 *)
let findDays : int -> int = fun month -> if month = 1 || month = 3 || month = 5 || month = 7 || month = 8 || month = 10 || month = 12 then 31 else if month = 4 || month = 6 || month = 9 || month = 11 then 30 else if month = 2 then 28 else -1;;
