@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [3 x i8] c"%d\00"	
@lf  = internal constant [4 x i8] c"%lf\00"	

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)

define void @_f_printInt(i32 %x) {
entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...)* @printf(i8* %t0, i32 %x) 
	ret void
}

define void @_f_printDouble(double %x) {
entry: %t0 = getelementptr [6 x i8]* @fnl, i32 0, i32 0
	call i32 (i8*, ...)* @printf(i8* %t0, double %x) 
	ret void
}

define void @_f_printString(i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

define i32 @_f_readInt() {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)
	%t2 = load i32* %res
	ret i32 %t2
}

define double @_f_readDouble() {
entry:	%res = alloca double
        %t1 = getelementptr [4 x i8]* @lf, i32 0, i32 0
	call i32 (i8*, ...)* @scanf(i8* %t1, double* %res)
	%t2 = load double* %res
	ret double %t2
}

declare i8* @calloc(i32, i32)


%arrayLink = type {i32, [0 x i8*]}

define i8* @multiArray([0 x i32]* %sizes, i32 %last, i32 %finalsize, i32 %index) {
entry:
  %0 = getelementptr [0 x i32]* %sizes, i32 0, i32 %index
  %size = load i32* %0
  %arr = alloca %arrayLink*
  %1 = icmp slt i32 %index, %last
  br i1 %1, label %case0, label %case1
case0:
  %2 = mul i32 %size, 8
  %3 = add i32 %2, 4
  %4 = call i8* @calloc(i32 1, i32 %3)
  %arr1 = bitcast i8* %4 to %arrayLink*
  store %arrayLink* %arr1, %arrayLink** %arr
  %len = getelementptr %arrayLink* %arr1, i32 0, i32 0
  store i32 %size, i32* %len
  %index1 = add i32 %index, 1
  %i = alloca i32
  store i32 0, i32* %i
  br label %check
case1:
  %5 = mul i32 %size, %finalsize
  %6 = add i32 %5, 4
  %7 = call i8* @calloc(i32 1, i32 %6)
  %arr2 = bitcast i8* %7 to %arrayLink*
  store %arrayLink* %arr2, %arrayLink** %arr
  %len1 = getelementptr %arrayLink* %arr2, i32 0, i32 0
  store i32 %size, i32* %len1
  br label %end
check:
  %i1 = load i32* %i
  %8 = icmp slt i32 %i1, %size
  br i1 %8, label %loop, label %end
loop:
  %9 = call i8* @multiArray([0 x i32]* %sizes, i32 %last, i32 %finalsize, i32 %index1)
  %10 = getelementptr %arrayLink* %arr1, i32 0, i32 1, i32 %i1
  store i8* %9, i8** %10
  %i2 = add i32 %i1, 1
  store i32 %i2, i32* %i
  br label %check
end:
  %arr3 = load %arrayLink** %arr
  %11 = bitcast %arrayLink* %arr3 to i8*
  ret i8* %11
}
