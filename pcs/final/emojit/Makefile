run:
	./emojit -run input.txt

emojit_patched: emojit.c
	gcc -O3 $^ -o $@

clean:
	rm -rf emojit_patched
