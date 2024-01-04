FACTORIAL:
  $ dune exec demoInterpreter << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let x = fac 6
  > EOF
  val fac : int -> int = <fun>
  val x : int = 720
FIBONACCI:
  $ dune exec demoInterpreter << EOF
  > let rec fib = fun n -> match n with 
  > | 0 -> 0
  > | 1 -> 1 
  > | n -> fib (n - 1) + fib (n - 2) 
  > let x = fib 10
  > EOF
  val fib : int -> int = <fun>
  val x : int = 55
REVERESE LIST:
  $ dune exec demoInterpreter << EOF
  > let rev list =
  >   let rec helper acc list =
  >     match list with
  >     | [] -> acc
  >     | h :: tl -> helper (h :: acc) tl
  >   in
  >   helper [] list
  > let reversed1 = rev [1;2;3;4;5]
  > let reversed2 = rev [true;false;false;false]
  > EOF
  val rev : 'a list -> 'a list = <fun>
  val reversed1 : int list = [5; 4; 3; 2; 1]
  val reversed2 : bool list = [false; false; false; true]
FORMAT STRINGS, PRINTF, ETC
  $ dune exec demoInterpreter << EOF
  > let fmt1 = format_of_string "int: %d, char: %c" 
  > let fmt1 = "string: %s" ^^ fmt1
  > let fmt1 = format_of_string fmt1
  > let str = "abcdef\n";;
  > printf fmt1 str (length str) str.[0]
  > EOF
  string: abcdef
  int: 7, char: a
  val fmt1 : string -> int -> char -> unit format_string = "string: %sint: %d, char: %c" format
  val str : string = "abcdef
  "
