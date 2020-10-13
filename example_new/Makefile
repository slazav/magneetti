# Calculate coil system using test.txt description,
# plot result. It is same calculation as in `example_orig`,
# but using `magnet_new` program instead of magnet+magneettiSH
# combination.

all: test.plot test.fld
	./test.plot

%.fld: %.txt
	../magnet_new < test.txt > test.fld

%.plot: %.txt
	../magneetti_plot test.txt

clean:
	rm -f *.bak *.fld *.plot
