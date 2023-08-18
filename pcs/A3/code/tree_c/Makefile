all: tree
.PHONY: tree debug hugtree.o # all abc careful hugtree

tree: ../hugtree.o tree.c
	gcc -g3 -no-pie $^ -o $@

../hugtree.o: ../hugtree.asm
	nasm -f elf64 $< -o $@

hugtree: hugtree.asm
	nasm -o $@ $<

debug: tree tree_debug.gdb
	gdb --command=tree_debug.gdb ./tree

.PHONY: clean
clean:
	rm -f hugtree tree *.o

