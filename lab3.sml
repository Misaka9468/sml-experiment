fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun printArray ( Arr ) =
    let
	val cur = ref 0
	val len = Array.length(Arr)
    in
	while !cur < len
	do
	(
	  printInt(Array.sub(Arr,!cur));
	  cur := !cur + 1)
    end;

fun printString( s ) = print(s ^ " ");

(*****Begin*****)
(*菜鸡O(n^2)算法*)		 
(*通过对left值变化的判断，确定闭合*)
fun parens([], len, left)=
	if(left=0) then len   (*判断到串结束*)
		else 0
	|parens(x::xlist, len:int, left:int)=
		if(left=0 andalso len>0) 
			then len
		else if(left<0) 
			then 0   (*右括号大于左括号 此时明显不匹配*)
		else if(x=1) 
			then parens(xlist, len+1, left-1)
		else parens(xlist, len+1, left+1);

fun getmax([],max) = max
	|getmax(x::xlist,max)=
		let	
			fun bigger(x,y) = 
				if x>y then x
				else y;
		in
 			getmax(xlist,bigger(max,parens(x::xlist,0,0)))
	end;

val num = getInt();
val list = getIntTable(num);

printInt(getmax(list, ~1));
(*****End*****)

