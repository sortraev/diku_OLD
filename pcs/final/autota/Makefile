

run: autota doit.py
	./doit.py | ./autota

debug: autota_debug.gdb autota
	gdb --command=autota_debug.gdb ./autota
