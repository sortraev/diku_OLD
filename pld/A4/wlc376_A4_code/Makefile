
# run the PLD-LISP interpreter with Troll loaded.
run: lisp.exe
	(printf "(load Troll)\n" && cat) | ./$<

runt: lisp.exe
	(printf "(load Torben)\n" && cat) | ./$<

# run the examples given in the assignment text.
example: lisp.exe
	printf "(load Troll)\n(load example)\n" | ./$<



# run all tests.
test: manual_tests unit_tests

manual_tests: lisp.exe manualTests.le
	printf "(load Troll)\n(load manualTests)\n" | ./$<

unit_tests: lisp.exe unitTests.le
	printf "(load Troll)\n(load unitTests)\n" | ./$<



lisp.exe: RunLISP.fsx Sexp.dll
	fsharpc $< -o $@ -r Sexp.dll

Sexp.dll: Sexp.fs
	fsharpc -a $<

clean:
	rm -f lisp.exe lisp2.exe Sexp.dll FSharp.Core.dll
