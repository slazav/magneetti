all: test.plot test.fld
	./test.plot

%.cmd: %.txt
	../magneettiSH test.txt

%.plot: %.txt
	../magneetti_plot test.txt

%.fld: %.cmd
	../magnet < test.cmd > /dev/null

clean:
	rm -f *.bak *.cmd *.fld *.dsc *.plot
