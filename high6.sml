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

(*flag = 1 表示进位*)
fun plus([],[],flag)=
	    if(flag=0) then []
	    else 1::[]
    |plus(x1::T1,[],flag)=
	    if(x1+flag<=9) then (x1+flag)::plus(T1,[],0)
	    else (x1+flag-10)::plus(T1,[],1)
    |plus([],x2::T2,flag)=
	    if(x2+flag<=9) then (x2+flag)::plus([],T2,0)
	    else (x2+flag-10)::plus([],T2,1)
    |plus(x1::T1,x2::T2,flag)=
	    if(x1+x2+flag<=9) then (x1+x2+flag)::plus(T1,T2,0)
	    else (x1+x2+flag-10)::plus(T1,T2,1);

(*flag = 1表示退位*)
fun minus([],[],flag) = []
    |minus([],x2::T2,flag)=[]
    |minus(x1::T1,[],flag)=
        if(x1-flag<0) then (x1-flag+10)::minus(T1,[],1)
			else (x1-flag)::minus(T1,[],0)
    |minus(x1::T1,x2::T2,flag)=
        if(x1-x2-flag<0) then (x1-x2-flag+10)::minus(T1,T2,1)
			else (x1-x2-flag)::minus(T1,T2,0);

(*一个长整数乘以一个数*)
fun time([],_,flag)=flag::[]
    |time(x1::T1,num,flag)=
	    if((x1*num+flag)<=9) then (x1*num+flag)::time(T1,num,0)
	    else ((x1*num+flag) mod 10)::time(T1,num,(((x1*num+flag)-(x1*num+flag) mod 10)div 10));

(*将每次得到的乘法结果移位*)
fun movebit(T,length)=
	    if(length>0) then 0::movebit(T,length-1)
	    else T;

fun times([],_,ans,length)=ans
    |times(_,[],ans,length)=ans
    |times(T1,x2::T2,ans,length)=
	    times(T1,T2,plus(ans,movebit(time(T1,x2,0),length),0),length+1);

(*第一个参数为要转换的，第二个为空表*)
fun inverse([],list)=list    
    |inverse(x::xlist,list)=inverse(xlist,x::list);


(*为了消除开头的0  遇到个有效数字，就设置为0*)
fun specialprint([],_)=()
    |specialprint(0::[],1)=
        printInt(0)
    |specialprint(0::xs,1)=
        specialprint(xs,1)
    |specialprint(x::xs,_)= 
        let 
            val tmp=printInt(x)
        in
            specialprint(xs,0)
        end;

fun printans(T1:int list,T2:int list) = 
    let 
        val ans1 = inverse(plus(T1,T2,0),[]);
        val ans2 = inverse(minus(T1,T2,0),[]);
        val ans3 = inverse(times(T1,T2,(0::[]),0),[]);
        val t1 = specialprint(ans1,1);
        val t2 = printEndOfLine();
        val t3 = specialprint(ans2,1);
        val t4 = printEndOfLine();
        val t5 = specialprint(ans3,1);
    in
        0
    end;

val N1=getInt();
val T1=inverse(getIntTable(N1),[]);
val N2=getInt();
val T2=inverse(getIntTable(N2),[]);

printans(T1,T2);

(*****End*****)
