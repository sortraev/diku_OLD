# Remove the blank-handin and add your report once you start working
# on a task.

.PHONY: handin.zip

AUTOTA_FILES = \
	autota/autota.md \
	autota/doit.py \
	autota/autota_disas_commented.asm

EMOJIT_FILES = \
	emojit/emojit.md \
	emojit/exploit.txt

GOTLESS_FILES = \
	gotless/gotless.md \
	gotless/doit.py \
	gotless/rop_stuff.py

PENTADECIMAL_FILES = \
	pentadecimal/pentadecimal.md \
	pentadecimal/shellcode.asm

UNDETECTED_FILES = \
	undetected/undetected.md \
	undetected/doit.py \
	undetected/undetected_reversed.c \
	undetected/undetected_disas_commented.asm



handin.zip: ${AUTOTA_FILES} ${GOTLESS_FILES} ${PENTADECIMAL_FILES} \
            ${UNDETECTED_FILES} ${EMOJIT_FILES}
	rm -f handin.zip
	@echo "!!! Remember to update the Makefile to include your reports !!!"
	zip -MM $@ $^


.PHONY: onlineta
onlineta: handin.zip
	curl -F handin=@handin.zip https://pcs.incorrectness.dk/grade/final
