fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntInf (a:IntInf.int) =
    print(IntInf.toString(a)^" ");


fun printReal (a:real) =
    print(Real.toString(a)^" ");

fun printString (a:string) =
    print(a^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun getIntInf () =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);

fun getReal () =
    Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);

fun printEndOfLine () =
    print("\n");

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun printIntInfTable ( [] ) = ()
  | printIntInfTable ( x::xs ) = 
    let
	val tmp = printIntInf(x)
    in
	printIntInfTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun getIntInfTable ( 0 ) = []
  | getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);

fun getIntVector ( 0 ) =  Vector.fromList []
  | getIntVector ( N:int) = Vector.fromList(getIntTable(N));

fun getIntInfVector ( 0 ) = Vector.fromList []
  | getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));


(*****Begin*****)
(*快速幂*)
fun quickpow(A:IntInf.int,1,M:IntInf.int) :IntInf.int = A mod M
    |quickpow(A:IntInf.int,N:IntInf.int,M:IntInf.int) :IntInf.int = 
	    if N mod 2 =0 
            then quickpow((A mod M)*(A mod M), N div 2, M) mod M
			else ((A mod M)*quickpow((A mod M)*(A mod M), N div 2, M)) mod M;

(* 接收n-1，将其分解为2^q·m *)
fun getQandM(n:IntInf.int,q:IntInf.int) : IntInf.int*IntInf.int = 
    if (n mod 2) <> 0
     then (q,n)
    else getQandM(n div 2, q+1); 

(* 获得符合大小的"随机"数 *)
fun getrandom(num:IntInf.int,n:IntInf.int) = num mod (n-1) + 1; 


(*check单次，接收n,底数a,指数q,指数m,次数记录i,返回1表示通过Miller测试，返回0表示未通过*)
fun check(n:IntInf.int,a:IntInf.int,q:IntInf.int,m:IntInf.int,i:IntInf.int) = 
    if i = q then 0 (*所有都没通过测试，否则在之前就会退出*)
    else
        let
            val ans = quickpow(a,m,n);
        in
            if ans = 1 andalso i = 0 then 1
            else if ans = n-1
                then 1
            else check(n,a,q,2*m,i+1)    
        end;    

val n = getIntInf();
val numlist : IntInf.int list = [2,4,123122];(*选择3个数用作选随机数*)
val (q,m) = getQandM(n-1,0);

fun checklist([],N,q,m) = 1
    |checklist(x::xs,N,q,m) = 
        if(check(N,getrandom(x,N),q,m,0) = 0)
            then 0
        else checklist(xs,N,q,m);    

fun result(numlist,N,q,m)=
    if N =2 then print("True")
    else if(checklist(numlist,N,q,m) = 1) then print("True")
	else print("False");

result(numlist,n,q,m);   
(*****End*****)

