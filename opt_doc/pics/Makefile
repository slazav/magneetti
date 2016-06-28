pics = fits geom

all: ${pics:=.tex} ${pics:=.eps}\
     ${pics:%=full_%.eps}
#all: ${pics:=.tex} ${pics:=.eps}

%.png: %.fig
	fig2dev -Lpng -j -m3.75 $< $@

%.eps: %.fig
	fig2dev -Lpstex -m1 -j $< $@
%.tex: %.fig
	fig2dev -Lpstex_t -m1 -p ${@:.tex=.eps} -j $< $@

#txt_%.eps: %.fig
#	fig2dev -Leps -m1 -j $< $@
full_%.eps: %.tex
	epstex2eps $<

clean:
	rm -f *.tex 0*.png *.eps *.bak tmp_fig.*
