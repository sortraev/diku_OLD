main=main
src=$(main).tex
pdf=$(main).pdf

.PHONY: run view clean clean-all

$(pdf): $(src) 1.tex
	pdflatex -shell-escape $<
	# bibtex $(main).aux
	# pdflatex -shell-escape $<
	# pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
