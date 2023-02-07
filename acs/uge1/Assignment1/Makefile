javac_flags=-g

src=assignment1
package=$(src)

main=Run

default: run

run: compile
	java -cp out $(package).$(main)

compile:
	javac $(javac_flags) -d "out" $(src)/Run.java

clean:
	rm -rf out
