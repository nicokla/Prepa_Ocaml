type nb = int of int | flo of float | fra of int * int | com of int*int |modn of int
|pom (*plus omega*) |mom (*moins omega*) |und (*undefined*);;
type pol == nb vect;;
type mat == nb vect vect;;
let rec prefix $+ n1 n2 = match n1, n2 with
    int (a), flo (b) -> flo ((float_of_int a) +. b)
    | int (a), int (b) -> int (a + b)
    | flo (a), int (b) -> flo ((float_of_int b) +. a)
    | flo (a), flo (b) -> flo (a +. b)
		|(*...*);;
