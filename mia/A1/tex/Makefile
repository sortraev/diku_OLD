main=main
src=$(main).tex 1.tex 2.tex commands_and_packages.tex
pdf=$(main).pdf

.PHONY: $(pdf) clean clean-all

$(pdf): $(src)
	pdflatex -shell-escape -draftmode $<
	pdflatex -shell-escape $<

clean:
	rm -rf *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf
