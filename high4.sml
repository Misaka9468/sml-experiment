fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val BeforeHeight = printInt(x)
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
val N=getInt();
val buildings=Array2.array(10001,3,~1);

fun ReadInput(buildings,N) = 
    let
        val times=ref 0;
        val tmp1=ref 0;
        val tmp2=ref 0;
        val tmp3=ref 0;
    in
        while(!times<N) do(
	        tmp1:=getInt();tmp2:=getInt();tmp3:=getInt();
	        Array2.update(buildings, !times,0, !tmp1);
	        Array2.update(buildings, !times,1, !tmp2);
            Array2.update(buildings, !times,2, !tmp3);
	        times:= !times+1)
    end;

(*合并两个building的操作 len1最大*)
fun Merge(first,second,len1,len2)=
	let
		val final=Array.array(Array.length(first),0);
		val index=ref 0;
	in
		(
        while !index<len2   
            do(
				if(Array.sub(first,!index)>Array.sub(second,!index)) 
                    then Array.update(final,!index,Array.sub(first,!index))
				else Array.update(final,!index,Array.sub(second,!index));
				index:= !index+1
			);
		while !index<len1
			do(
				Array.update(final,!index,Array.sub(first,!index));
				index:= !index+1
			);
		final
        )
	end;
		

fun MergeControl(building1,building2) = 
    let 
        val len1 = Array.length(building1);
        val len2 = Array.length(building2);
        in
            if(len1>len2) then Merge(building1,building2,len1,len2)
            else Merge(building2,building1,len2,len1)
        end;

fun Divide(buildings,N)=
    let
        fun GetAns(Ans,left,height,right,index)=  (*将一个三元组变成显示高度、左右的一维数组*)
	        if(index<right-1) then 
                if(index>left-2) then 
                    (   Array.update(Ans,index,height);
					    GetAns(Ans,left,height,right,index+1))
				else GetAns(Ans,left,height,right,index+1)
	    else Ans;
        
        fun Copy(target,source,begin,termination,index) =
	        if(begin <= termination) then 
                (   Array2.update(target,index,0,Array2.sub(source,begin,0));
                    Array2.update(target,index,1,Array2.sub(source,begin,1));
                    Array2.update(target,index,2,Array2.sub(source,begin,2));
			        Copy(target,source,begin+1,termination,index+1))
	        else target;
    in    
	    if(N=1) then 
		    (let
                val left=Array2.sub(buildings,0,0);
                val height=Array2.sub(buildings,0,1);
                val right=Array2.sub(buildings,0,2);
		    	val Ans=Array.array(Array2.sub(buildings,0,2),0);
		    in
	    	    GetAns(Ans,left,height,right,0)
		    end)
	    else if (N mod 2 = 0) then 
		    (let
			    val LeftPart=Array2.array(N div 2,3,0);
    		    val RightPart=Array2.array(N div 2,3,0);
			    val t1=Copy(LeftPart,buildings,0,N div 2 -1,0);
			    val t2=Copy(RightPart,buildings,N div 2,N-1,0);
		    in
			    MergeControl(Divide(LeftPart,N div 2),Divide(RightPart,N div 2))	
		    end)
        else
		    (let
			    val LeftPart=Array2.array((N-1)div 2,3,0);
			    val RightPart=Array2.array((N+1)div 2,3,0);
                val t1=Copy(LeftPart,buildings,0,(N-1)div 2 -1,0);
			    val t2=Copy(RightPart,buildings,(N-1)div 2,N-1,0);
		    in
			    MergeControl(Divide(LeftPart,(N-1)div 2),Divide(RightPart,(N+1)div 2))
		    end)
    end;

fun PrintAns(buildings,N) = 
    let
        val times = ref 0;
        val BeforeHeight = ref 0;
        val Ans = Divide(buildings,N);
        val length = Array.length(Ans);
    in
        while( !times < length) do
        (
	        if(Array.sub(Ans, !times) <> !BeforeHeight) (*出现突变*)
                then 
                (   printInt( !times+1);
                    printInt(Array.sub(Ans, !times));
                    print("\n")
                )
	        else ();
	        BeforeHeight:= Array.sub(Ans, !times);
            times:= !times+1
        )
    end;

ReadInput(buildings,N);
PrintAns(buildings,N);

(*****End*****)
