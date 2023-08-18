main=main
src=$(main).tex 1.tex 2.tex 3.tex
pdf=$(main).pdf

.PHONY: run view clean clean-all

$(pdf): $(src) commands_and_packages.tex
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
