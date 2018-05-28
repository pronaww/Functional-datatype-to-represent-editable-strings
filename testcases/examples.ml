let nil = create "";;
let a = create "a";;
let abc = create "abc";;
let twelve = create "12";;
let oneA = create "1A";;
let one = create "1";;

nonempty nil;;
nonempty a;;
nonempty abc;;
nonempty twelve;;

concat nil nil;;
concat nil a;;
concat one nil;;
concat oneA abc;;

reverse nil;;
reverse abc;;
reverse twelve;;

first nil;;
first a;;
first abc;;

last nil;;
last a;;
last abc;;

let editable = create "abac12a2aAac211";;
forward editable;;
back editable;;
moveTo 10 editable;;
replace (Letter 'b') editable;;
