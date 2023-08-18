.PHONY: all week1.md
all: strange ternary_tests slurp_tests


ternary.o: ternary.asm
	nasm -f elf64 -o $@ $<

ternary_tests: ternary.o ternary_tests.c
	gcc -g3 $^ -o $@


main: slurp.o main.c
	gcc -g3 -no-pie $^ -o $@


slurp.o: slurp.asm
	nasm -g3 -F dwarf -f elf64 -o $@ $<

slurp_tests: slurp.o slurp_tests.c
	gcc -g3 -no-pie $^ -o $@



strange.o: strange.asm
	nasm -f elf64 -g -o $@ $<

strange: strange.o
	gcc -no-pie strange.o -o strange

debug_strange: strange
	gdb --command=strange_debug.gdb ./$<


week1.md: ../week1.md
	cp $< .

# Some rules to start off for submitting code for OnlineTA
# Remember to add your report
handin.zip: Makefile slurp_tests.c ternary_tests.c slurp.asm strange.asm strange_debug.gdb ternary.asm week1.md
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -MM $@ $^
	rm ./week1.md


.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week1

.PHONY: clean
clean:
	rm -f *.o strange ternary_tests slurp_tests strange_gdb_debug.output
