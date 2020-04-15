(*gestion du temps*)
#open "sys";;

let wait t (*en s*) =
let k = time() in
while time() < k+.t do () done;;
