open Array
exception Empty
exception AtLast
exception AtFirst
exception TooShort

type a = Letter of char;;

type a_star = {mutable marker: int; arr: a array}

let create s =
let b = Array.make (String.length s) (Letter(' ')) in
for i=0 to (String.length s - 1) do
	b.(i)<- Letter s.[i]
done;
{marker = 0; arr = b}

let lgh s = length s.arr

let nonempty s = if length s.arr > 0 then true else false

let concat s1 s2 = match s1 with
	{marker = i ; arr = a} -> match s2 with
				{marker = j ; arr = b} -> let c = Array.make (Array.length a + Array.length b) (Letter(' '))  in
											if (Array.length a)>0 then
											for i = 0 to Array.length a - 1 do
													c.(i)<-a.(i)
											done;
											if (Array.length b)>0 then
											for i = Array.length a to Array.length a + Array.length b -1 do
													c.(i)<-b.(i - Array.length a)
											done;
											{marker = 0; arr = c}

(*
Complexity of concat = O((length of s1)+(length of s2)).

Proof:
lgh(s1) = length of array containing the letters of s1
lgh(s2) = length of array containing the letters of s2

concat s1 s2 gives an array of length = length of array containing the letters of s1 + length of array containing the letters of s2
which is stored in the datatype 'a_star'

Since lgh(s) = length of array containing the letters of s
lgh(concat s1 s2) = length of array containing letters
				  = length of array containing the letters of s1 + length of array containing the letters of s2
				  = lgh(s1) + lgh(s2)
*)

let reverse s =
	let a = s.arr and b = Array.make (Array.length s.arr) (Letter(' ')) in
	let rec rev i arr =
					if i < (Array.length a) then b.(i)<-a.((Array.length a)-i-1);
					if i < (Array.length a) then rev (i+1) b
					else arr in
					{marker = 0; arr = rev 0 b}
(*
Proof:
	lgh(s) = length of array containing the letters of s
	lgh(reverse s) = length of array containing the letters of reverse s
				   = length of array containing the letters of s [Since length of arr of reverse s = length of arr of s]
				   = lgh(s)
*)				   

let first s = match s with
	{marker = i ; arr = a} ->  if (length a)>0 then a.(0) else raise Empty

let last s = match s with
	{marker = i ; arr = a} ->  if (length a)>0 then a.((length a)-1) else raise Empty

let forward s =
	if s.marker<length s.arr - 1 then
	s.marker<-s.marker+1
	else raise AtLast;
	s

let back s =
	if s.marker > 0 then
	s.marker<-s.marker-1
	else raise AtFirst;
	s

let moveTo n s =
	if n >= lgh s then raise TooShort
	else s.marker <- n;
	s
	(*
	Complexity of moveTo = O(1).
	*)

let replace w s =
	s.arr.(s.marker)<-w;
	s;;

(*
Proof:
	lgh(s) = length of array containing the letters of s
	Since the length of arr of s remains same
	Therefore after replace w s, length of arr of s will remain same
	which implies lgh(replace w s)=lgh(s)
*)
