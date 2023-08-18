.PHONY: setup
setup: shellcode

shellcode: shellcode.asm
	nasm -o shellcode shellcode.asm

week4.md: ../week4.md
	cp $< .

# Some rules to start off for submitting code for OnlineTA
# Remember to add your report and support files (if any)
handin.zip: doit_waldo.py doit_celeb.py doit_new_absalon.py celeb_exploit.b shellcode.asm Makefile week4.md
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -MM $@ $^
	rm ./week4.md

.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week4
