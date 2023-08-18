.PHONY: run

run: ./bomb_overrides.so ./bomb solution.txt
	cat solution.txt | LD_PRELOAD=./bomb_overrides.so ./bomb 

# Some rules to start off for submitting code for OnlineTA
# Remember to add your report and support files (if any)
handin.zip: bomb-assembly.asm solution.txt week2.md c_implementations
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -r -MM $@ $^
	rm ./week2.md

week2.md: ../week2.md
	cp $< .

debug: bomb_overrides.so bomb bomb_debug.gdb
	gdb --command=bomb_debug.gdb ./bomb

bomb_overrides.so: bomb_overrides.c
	gcc -Wall -Wno-pedantic -shared -ldl -fPIC -o $@ $< # -DENABLE_CHEATS


.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week2
