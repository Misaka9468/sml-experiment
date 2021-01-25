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

val Dfn=Array.array(N+1,0);
val Low=Array.array(N+1,0);
val Head=Array.array(N+1, ~1);
val Edge=Array2.array(M*2,2,0);
val Flag=Array.array(N+1,0);

val DfsClock=ref 0;
val PointCnt=ref 0;
val EdgeCnt=ref 0;

fun Tarjan(Flag,EdgeCnt,DfsClock,cur,father)=
    let
        fun min(x,y) = if(x>y) then y else x ;
        val tmp=(DfsClock:= !DfsClock+1);    
        (*打上时间戳*)
    	val t1=Array.update(Dfn,cur, !DfsClock);
		val t2=Array.update(Low,cur, !DfsClock);
		val index=ref (Array.sub(Head,cur));
        val vertex=ref 0;
        val children = ref 0;
    in
		while( !index <> ~1)
    	do(
        	vertex :=Array2.sub(Edge, !index,1); (*寻找邻居节点*)
        	if(Array.sub(Dfn, !vertex)=0)  (*未曾访问v*)
        	then(
            	children:= !children+1;
            	Tarjan(Flag,EdgeCnt,DfsClock, !vertex,cur);
            	Array.update(Low,cur,min(Array.sub(Low,!vertex),Array.sub(Low, cur)));
                (*不是自己且满足割点条件且未被访问*)
            	if(cur <> 1 andalso Array.sub(Dfn,cur) <= Array.sub(Low, !vertex) ) then Array.update(Flag,cur,1)  
            	else if(cur = 1  andalso !children >= 2) then Array.update(Flag,cur,1)
				else ();
                (*满足割边条件*)
            	if(Array.sub(Dfn, cur) < Array.sub(Low, !vertex)) then EdgeCnt:= !EdgeCnt+1
                else ()
			)
        	else if( !vertex <> father)
                then Array.update(Low,cur,min(Array.sub(Dfn, !vertex),Array.sub(Low,cur)))
			else ();
			index := Array2.sub(Edge, !index,0)
		)
    end;

fun Init(Edge,Head,M) = 
    let 
        val u=ref 0;
        val v=ref 0;
        val times=ref 0; 
        val num=ref 0;
        fun Add(x,y,num)=
	        let 
		        val t1=Array2.update(Edge, !num,1,y);
		        val t2=Array2.update(Edge, !num,0,Array.sub(Head,x));
		        val t3=Array.update(Head,x, !num);
	        in
		        num:= !num+1
	        end;
        fun AddEdge(x,y,num)=
	        let 
		        val t1=Add(x,y,num);
		        val t2=Add(y,x,num);
	        in 
		        ()
	        end;        
    in     
        while( !times < M) do
        (
            u:=getInt();v:=getInt();
            AddEdge( !u, !v, num);
	        times:= !times+1
        )
    end;

fun GetAns(Edge,Head,Flag,PointCnt,EdgeCnt,DfsClock,N,M) = 
    let 
        val t1 = Init(Edge,Head,M);
        val t2 = Tarjan(Flag,EdgeCnt,DfsClock,1,1);
        val time = ref 1;
    in
        while( !time<=N)
        do(
	        if(Array.sub(Flag, !time)=1) then PointCnt:= !PointCnt+1
	        else ();
	        time:= !time+1
          )
    end;

GetAns(Edge,Head,Flag,PointCnt,EdgeCnt,DfsClock,N,M);

printInt(!PointCnt);
printInt(!EdgeCnt);
(*****End*****)

