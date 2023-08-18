main=main
src=$(main).tex 1.tex 5_1.tex 5_2.tex 5_3.tex
pdf=$(main).pdf

.PHONY: $(pdf) clean clean-all

$(pdf): main.tex #% commands_and_packages.tex
	pdflatex -shell-escape -draftmode $<
	pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
