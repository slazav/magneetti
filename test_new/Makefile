all: test.plot test.fld
	./test.plot

%.plot: %.txt
	../magneetti_plot test.txt

%.fld: %.txt
	../magnet_new < test.txt > test.fld

clean:
	rm -f *.bak *.fld *.plot
