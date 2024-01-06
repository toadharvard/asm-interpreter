  $ dune exec demoInfer << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let sum x y = x + y
  > let id x = x 
  > let f x = match id x with
  > | a::b::c -> let s = sum a b in printf "%d" s; s
  > | a::b -> printf "%d" a; fac a 
  > | [] -> printf "Achieved empty\n"; 1 
  > EOF
  val f : forall [ ] . int list -> int
  val fac : forall [ ] . int -> int
  val id : forall [ 11; ] . '_11 -> '_11
  val sum : forall [ ] . int -> int -> int
PRINTF
  $ dune exec demoInfer << EOF
  > let my_printf = printf
  > let fmt_printf = my_printf ("string: %s\n" ^^ format_of_string "int: %d\n");;
  > fmt_printf "abcd" 123
  > EOF
  val fmt_printf : forall [ ] . string -> int -> unit
  val my_printf : forall [ 2; ] . '_2 format_string -> '_2
