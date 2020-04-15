
TD de programmation No 1. (le 14 octobre 1994)




let max2 x y = if x>y then x else y;;

let max (x,y) = if x>y then x else y;;





let x = "hello";;

x;;

let y = "Bonjour" in y ^" a tous.";;

y;;

x + 1;;

let x = 4 in x + 1;;

x + 1 where x = 3;;

x;;

let x = 5;;

x + 1;;





let x = 3
  in let y = x + 1
    in x + y;;

let x = 3 and y = x + 1
  in x + y;;





let x=3 in
  let f z = z+x in
    let x = true in
f 7;;

let x=true in
  let f z = z+x in
    let x = 3 in
f 7;;





Fibonnaci:

let rec fibo = fun 0 -> 1 
                 | 1 -> 1 
                 | n -> fibo(n-1) + fibo(n-2);;

let fibo2 n = fst(fibo_dbl n)
  where rec fibo_dbl = fun 0 -> (1,0)
                         | n -> (x+y,x)   where (x,y)=fibo_dbl(n-1);;





Pgcd:

let rec pgcd (x,y) = if y mod x = 0 then x
                                    else pgcd(y,x mod y);;




Curry, Uncurry:

let curry f = fun x -> fun y -> f(x,y);;
let uncurry g = fun (x,y) -> g x y;;

let pgcd24 = curry pgcd 24;;





Racine carree:

let rec root (a,epsilon,essai) = if abs_float(essai*.essai-.a)<.epsilon 
                                 then essai
                                 else root (a,epsilon,(essai+.a/.essai)/.2.0);;




Derivation:

let deriv (f,x,dx) = (f(x+.dx)-.f(x))/.dx;;
 



Newton:

let rec zero (f,dx,epsilon,essai) = 
        let y = essai -. f(essai)/.deriv(f,essai,dx)
        in if abs_float(y-.essai)<.epsilon then y
                                           else zero (f,dx,epsilon,y);;
 

zero ((fun x -> x*.x-.2.),0.0000001,0.00001,0.);;

- : float = 1.41421356237

zero (cos,1e-10,1e-8,1.0);;

1.57079632679



