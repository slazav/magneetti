# disser latex packaged is used:
#   http://www.ctan.org/tex-archive/macros/latex/contrib/disser/
#   http://sourceforge.net/projects/disser/


all: opt.pdf

%.pdf: %.ps
	ps2pdf $<

%.ps: %.dvi
	dvips $<
%.dvi: %.tex
	latex $<

clean:
	rm -f *.aux *.log *.bbl *.blg *.dvi *.ps *.pdf *.toc *.ind *.ilg
distclean: clean
	rm -f *.pdf

spell:
	ispell -d russian-lebedev ${parts_diss:=.tex} ${parts_auto:=.tex} otz_ved.tex auto.tex
