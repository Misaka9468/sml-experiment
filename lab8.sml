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
val N=getInt();
val M=getInt();

fun findlog(n,index) = 
    if n div 2 > 0 then findlog (n div 2,index+1)
    else index;

val k= findlog(N,0) +1;
val dp :int Array2.array = Array2.array(N+1,k,0);

val initlist1 = getIntTable(N);
val initlist = [0]@initlist1;
fun getmax(x,y) = if x>y then x else y;
fun getpower(x,ans,0) = ans
    |getpower(x,ans,y) = getpower(x,ans*x,y-1);
val j= ref 0;
val i= ref 1;
while (getpower(2,1,(!j))<=N) do 
    (   i:=1;
        while (!i+getpower(2,1,!j)-1 <= N) do 
            (   
                if(!j = 0) then Array2.update(dp,!i,!j,List.nth(initlist,!i)) 
                    else Array2.update(dp,!i,!j,getmax(Array2.sub(dp,!i,(!j-1)),Array2.sub(dp,(!i+getpower(2,1,!j-1)),(!j-1))));
                i:=(!i)+1
            );
        j:=(!j)+1
    );

fun request(times) = 
    if times > 0 then   
        let
            val l = getInt();
            val l1= if l=0 then 1 else l;
            val r = getInt();
            val len = r-l1+1;
            val k = findlog(len,0);
            val ans = getmax(Array2.sub(dp,l1,k),Array2.sub(dp,(r-getpower(2,1,k)+1),k));
            val tmp = printInt(ans);
        in 
            request(times-1)
        end    
    else 0;

request(M);

(*****End*****)

