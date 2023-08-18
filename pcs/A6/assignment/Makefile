REPORT=week6.md
TMPFILE := $(shell mktemp)
run: rop-chain-cat.py assignment libc.so.6
	# ./rop-chain-cool.py > $(TMPFILE)
	./rop-chain-cat.py > $(TMPFILE)
	cat $(TMPFILE) | LD_PRELOAD=./libc.so.6 ./assignment
	rm $(TMPFILE)


debug: assignment extras/assignment_debug.gdb
	gdb --command=extras/assignment_debug.gdb ./assignment

$(REPORT): ../$(REPORT)
	cp $< .

# Some rules to start off for submitting code for OnlineTA
# Remember to add your report and support files (if any)
handin.zip: rop-chain.txt rop-chain-fix.py rop-chain-cat.py rop-chain-cool.py common.py shellcode.asm $(REPORT)
	@echo "!!! Remember to update the Makefile to include your report !!!"
	zip -MM $@ $^
	rm ./$(REPORT)


.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/week6
