main=main
pdf=$(main).pdf

presentations_src=$(wildcard presentations/*.tex)
notes_src=$(wildcard notes/*.tex)
src=$(presentations_src) $(notes_src)

__pdflatex=pdflatex -shell-escape -output-directory ./out


.PHONY: run view clean clean-all

all: $(pdf)

presentations: ./out $(presentations_src)
	$(__pdflatex) -jobname=$@ '\def\$@{} \input{main}'
	$(__pdflatex) -jobname=$@ '\def\$@{} \input{main}'

notes: ./out $(notes_src)
	$(__pdflatex) -jobname=$@ '\def\$@{} \input{main.tex}'
	$(__pdflatex) -jobname=$@ '\def\$@{} \input{main.tex}'

$(pdf): ./out $(src)
	$(__pdflatex) -jobname=$(main) '\def\notes{}\def\presentations{}\input{main}'
	$(__pdflatex) -jobname=$(main) '\def\notes{}\def\presentations{}\input{main}'

./out:
	mkdir -p out

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.
	cp out/*.pdf . 2>/dev/null
	rm -rf out/*
