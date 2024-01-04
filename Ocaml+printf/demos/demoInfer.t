  $ dune exec demoInfer << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let sum x y = x + y
  > let id x = x 
  > let f x = match id x with
  > | a::b::c -> sum a b 
  > | a::b -> fac a 
  > | [] -> 1 
  > EOF
  val f : forall [ ] . int list -> int
  val fac : forall [ ] . int -> int
  val id : forall [ 11; ] . '_11 -> '_11
  val sum : forall [ ] . int -> int -> int
