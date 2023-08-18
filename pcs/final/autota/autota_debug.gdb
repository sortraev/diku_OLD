# main+286 is the `ret` we are going to use to jump to system("cat flag.txt")
b *(main+286)
commands
info frame
end

run <<< $(./doit.py)
