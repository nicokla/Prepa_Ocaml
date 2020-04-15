let retourne a =
let n = string_length a in
let b = create_string n in
for i = 1 to n do let k = a.[n-i] in
if k =`(` then b.[i-1]<-`)` else if
k =`)` then b.[i-1]<-`(` else if
k =`[` then b.[i-1]<-`]` else if
k =`]` then b.[i-1]<-`[` else if
k =`<` then b.[i-1]<-`>` else if
k =`>` then b.[i-1]<-`<` else if
k =`{` then b.[i-1]<-`}` else if
k =`}` then b.[i-1]<-`{` else 
b.[i-1] <- k done;b;;
