main=main
src=$(main).tex
pdf=$(main).pdf

.PHONY: run view clean clean-all

$(pdf): $(src) 1.tex 2.tex 2_resub.tex 3.tex 4.tex 5_resub.tex 5.tex 6.tex 7.tex packages.tex commands.tex
	pdflatex -shell-escape $<
	# bibtex $(main).aux
	# pdflatex -shell-escape $<
	# pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
