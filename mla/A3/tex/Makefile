main=main
out_dir=./out
pdf=$(out_dir)/$(main).pdf

.PHONY: $(pdf) clean clean-all

pdflatex_flags=-output-directory=$(out_dir) -shell-escape

$(pdf): $(main).tex #% commands_and_packages.tex
	pdflatex $(pdflatex_flags) -draftmode $< 
	pdflatex $(pdflatex_flags) $< 

clean:
	rm -rf out/* *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf

