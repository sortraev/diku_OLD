progs=Morse Nat Tree Move TicTacToe Size
ghc=ghc -dynamic

run_prog=TicTacToe
# run_prog=nat

run: $(run_prog)
	./$<

Morse: Morse.hs
	$(ghc) $@

Nat: Nat.hs
	$(ghc) $@

Tree: Tree.hs
	$(ghc) $@

Move: Move.hs
	$(ghc) $@

TicTacToe: TicTacToe.hs
	$(ghc) $@


Size: Size.hs
	$(ghc) $@


clean:
	rm -rf *.hi *.o
	rm -rf $(progs)
