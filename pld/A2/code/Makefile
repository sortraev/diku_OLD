
# task 1: lambda and lambdaD.

task1: lisp_v1.exe task1.le
	printf "(load task1)\n" | ./$<


# task 4: return statement in PLD-LISP - testing.

task4: test_v1 test_v2

test_v1: lisp_v1.exe
	printf "((lambda (x y) (+ x (return y))) 12 58)" | ./$<
	printf "((lambda (x y) (+ x (return y))) (return 12) 58)" | ./$<

test_v2: lisp_v2.exe
	printf "((lambda (x y) (+ x (return y))) 12 58)" | ./$<
	printf "((lambda (x y) (+ x (return y))) (return 12) 58)" | ./$<


# compilation.
Sexp.dll: Sexp.fs
	fsharpc -a $<

lisp_v1.exe: RunLISP_v1.fsx Sexp.dll
	fsharpc $< -o $@ -r Sexp.dll

lisp_v2.exe: RunLISP_v2.fsx Sexp.dll
	fsharpc $< -o $@ -r Sexp.dll


# cleanup.
clean:
	rm -rf lisp_v1.exe lisp_v2.exe Sexp.dll FSharp.Core.dll
