main=main
out_dir=./out
pdf=$(out_dir)/$(main).pdf

.PHONY: $(pdf) clean clean-all

pdflatex_flags=-output-directory=$(out_dir) -shell-escape

$(pdf): $(main).tex
	pdflatex $(pdflatex_flags) -draftmode $<
	pdflatex $(pdflatex_flags) $< 

$(out_dir):
	mkdir -p $(out_dir)

clean:
	rm -rf out/* *.pyg *.log *.aux *.out *.dvi *.fls *.fdb_latexmk _minted-* *.bbl *.blg # latexmk/pdflatex garbage.

