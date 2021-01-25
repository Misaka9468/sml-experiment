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

(*****Begin*****)

val num=getInt();

val original_list=getIntTable(num);

fun cmp(a, b) = if a < b then LESS else if a > b then GREATER else EQUAL;

fun merge (cmp , s:int list,t:int list) =
    let
        fun merge' [] t = t
            | merge' s [] = s
            | merge' (x::xs) (y::ys) =
                if cmp (y, x) = LESS
                    then y::merge' (x::xs) ys
                else x::merge' xs (y::ys)
    in merge' s t
    end;

fun merge_sort([]) = []
    | merge_sort([a]) = [a]
    | merge_sort(list:int list) = 
        merge(cmp,merge_sort(List.take(list,length list div 2)),merge_sort(List.drop(list,length list div 2)));

printIntTable(merge_sort(original_list));

(*****End*****)

