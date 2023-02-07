main=main
src=$(main).tex
pdf=$(main).pdf

.PHONY: run view clean clean-all

$(pdf): $(src) packages.tex commands.tex\
	    1.tex 1c.tex 2.tex 3.tex 3a.tex 3b.tex 3c.tex 4.tex 5.tex 6.tex
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
