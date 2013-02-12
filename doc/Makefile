LHS2TEX	= $(HOME)/.cabal/bin/lhs2TeX

all: presentation.pdf

%.pdf: %.tex
	pdflatex $<

%.tex: %.lhs
	$(LHS2TEX) -o $@ $<
