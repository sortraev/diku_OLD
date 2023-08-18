.PHONY: shellcode

debug: shellcode
	gdb -ex="run ./shellcode" runshellcode

shellcode: shellcode.asm
	nasm $^ -o $@
	# echo "echo >> hello from the popped shell" | runshellcode ./$@
	objdump -D -M intel -b binary -mi386:x86-64 shellcode
