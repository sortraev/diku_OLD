
run: lisp.exe assignment1.le
	printf "(load assignment1)\n" | ./$<

lisp.exe: compile.sh
	./$<

clean:
	rm -rf lisp.exe Sexp.dll FSharp.Core.dll
