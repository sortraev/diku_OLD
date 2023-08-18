
.PHONY: setup
setup: shellcode

shellcode: shellcode.asm
	nasm -o shellcode shellcode.asm

debug: debug.gdb
	gdb --command=debug.gdb ./new_absalon


# Some rules to start off for submitting code for OnlineTA
# Remember to add your report and support files (if any)
handin.zip: doit_waldo.py doit_celeb.py doit_new_absalon.py shellcode.asm Makefile
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -MM $@ $^

.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week4
