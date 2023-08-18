.PHONY: all abc careful hugtree clean onlineta
all: abc # careful hugtree

abc: abc.asm
	nasm -o abc abc.asm

careful: careful.asm
	nasm -o careful careful.asm

foo: foo.asm
	nasm -o foo foo.asm

hugtree: hugtree.asm
	nasm -o hugtree hugtree.asm


hugtree.o: hugtree.asm
	nasm -f elf64 $< -o $@



week3.md: ../week3.md
	cp $< .

# Some rules to start off for submitting code for OnlineTA
# Remember to add your report
handin.zip: Makefile abc.asm careful.asm hugtree.asm week3.md
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -MM $@ $^
	rm ./week3.md

onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week3

clean:
	rm -f abc careful hugtree handin.zip *.o
