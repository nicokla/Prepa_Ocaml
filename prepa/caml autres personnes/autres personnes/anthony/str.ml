#open "str";;

let re = regexp "\(a\|b\)*abb";;
string_match re "aabababc" 0;;
string_match re "aababbabc" 0;;