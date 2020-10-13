# Calculate coil system using test.txt description,
# plot result. Calculation is done in a few steps:
#
# * magneettiSH test.txt -- create `test.cmd`, answers
#   for original `magnet` program.
#
# * ../magnet < test.cmd > /dev/null -- run `magnet` program,
#   do the actual calculations. Produce `test.fld` -- field
#   profile table.
#
# * ../magneetti_plot test.txt -- produce a gnuplot script
#   for plotting results.
#
# * run gnuplot


all: test.plot test.fld
	./test.plot

%.cmd: %.txt
	../magneettiSH test.txt

%.fld: %.cmd
	../magnet < test.cmd > /dev/null

%.plot: %.txt
	../magneetti_plot test.txt

clean:
	rm -f *.bak *.cmd *.fld *.dsc *.plot
