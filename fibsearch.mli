type 'a minimizer = int -> int -> (int -> 'a) -> int * 'a
val minimize : 'a minimizer
val minimize_exhaustive : 'a minimizer
