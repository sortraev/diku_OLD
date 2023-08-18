main=main
out_dir=./out
pdf=$(out_dir)/$(main).pdf
pdflatex_flags=-output-directory=$(out_dir) -shell-escape

.PHONY: $(pdf) clean clean-all


$(pdf): $(main).tex
	pdflatex $(pdflatex_flags) -draftmode $< 
	pdflatex $(pdflatex_flags) $< 

$(out_dir):
	mkdir -p $@

clean:
	rm -rf out/* *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

clean-all: clean
	rm -rf *.pdf

