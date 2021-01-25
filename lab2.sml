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
(*Distance数组的值表示Ts与各顶点的距离*)
(*Flag数组的~1值表示是否连通*)
(*构造*)
val N=getInt();
val M=getInt();
val Ts=getInt();
val Rs=ref 0;
val Re=ref 0;
val Ci=ref 0;
val MAX=1024;
val Distance=Array.array(N+1,MAX);
val Edges=Array2.array(N+1,N+1,MAX);
val Flag=Array.array(N+1, ~1);
val times=ref 1;
Array.update(Flag,Ts,0);
Array.update(Distance,Ts,0);
while(!times<=M) do
(
	Rs:=getInt();
	Re:=getInt();
	Ci:=getInt();
    if(Array2.sub(Edges,!Rs,!Re)> !Ci) 
        then (Array2.update(Edges, !Re, !Rs, !Ci);Array2.update(Edges, !Rs, !Re, !Ci))
    else ();
    times:= !times+1
);

(*根据邻接表初始化Distance*)
fun InitDistance(Edges,Distance,N,Ts,0) = 0 
    |InitDistance(Edges,Distance,N,Ts,index)=
	    let
            val pos = N-index+1;
		    val tmp=Array.update(Distance,pos,Array2.sub(Edges,Ts,pos))
	    in
		    InitDistance(Edges,Distance,N,Ts,index-1)
	    end;

(*遍历Distance，找到合适节点 返回节点序号*)
fun GetNode(Distance,Flag,min,target,0) = target
    |GetNode(Distance,Flag,min,target,index)=
	    if(Array.sub(Flag,index)= ~1 andalso Array.sub(Distance,index)<min) 
            then GetNode(Distance,Flag,Array.sub(Distance,index),index,index-1)
	    else GetNode(Distance,Flag,min,target,index-1);

(*更新Distance*)
fun UpdateDistance(Edges,Distance,Flag,target,0) = 0
    |UpdateDistance(Edges,Distance,Flag,target,N)=
	    if(Array.sub(Flag,N)= ~1) then 
            let 
                val t1 = Array.sub(Distance,target)+Array2.sub(Edges,target,N);
                val t2 = Array.sub(Distance,N);
		        val min = if t1 > t2 then t2 else t1; 
                val tmp=Array.update(Distance,N,min);
	        in
		            UpdateDistance(Edges,Distance,Flag,target,N-1)
                end
        else 
            UpdateDistance(Edges,Distance,Flag,target,N-1);


fun Dijkstra(Edges,Distance,Flag,MAX,N,Ts,M)=
    if (N<=0) then 0
    else if(M<=0) then 0
    else if(Ts<=0) then 0
    else
        let
            fun function(Edges,Distance,Flag,MAX,N,0) = 0
                |function(Edges,Distance,Flag,MAX,N,index) =
	                let	
                        val target=GetNode(Distance,Flag,MAX,0,N);
                        val t1=Array.update(Flag,target,0);
		                val t2=UpdateDistance(Edges,Distance,Flag,target,N);
	                in	
		                function(Edges,Distance,Flag,MAX,N,index-1)
                    end;
            val t3 = InitDistance(Edges,Distance,N,Ts,N);
            val t4 = Array.update(Flag,Ts,0);
            val t5 = Array.update(Distance,Ts,0);
        in
            function(Edges,Distance,Flag,MAX,N,N)
        end;

fun PrintAns(Distance,Flag,index) = 
    if index > N then 0
    else 
        let 
            val tmp = if(Array.sub(Flag,index)= ~1) then printInt(~1)
	                  else printInt(Array.sub(Distance,index));
        in PrintAns(Distance,Flag,index+1)
        end;


Dijkstra(Edges,Distance,Flag,MAX,N,Ts,M);
PrintAns(Distance,Flag,1);

(*****End*****)

