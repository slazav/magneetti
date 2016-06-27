#FFLAGS= -Werror -Wconversion\
#  -Wline-truncation\
#  -Waliasing  -Wampersand -Warray-bounds -Wcharacter-truncation\
#  -Wline-truncation -Wsurprising -Wno-tabs -Wunderflow\
#  -Wno-unused-parameter -fPIC -fno-range-check -O\
#  -std=legacy


FFLAGS= -Wconversion\
  -Wline-truncation\
  -Waliasing  -Wampersand -Warray-bounds -Wcharacter-truncation\
  -Wline-truncation -Wno-tabs -Wunderflow\
  -Wno-unused-parameter -fPIC -fno-range-check -O\
  -std=legacy


all: magnet magnet_new libmagneetti.so

magnet: magnet.f magnet_lib.f magnnum.f
	g77 -O9 -o magnet $+

magnet_new: magnet_new.cpp magnet_lib.o magnnum.o
	g++ -O9 -o magnet_new $+ -lgfortran

libmagneetti.so: magnet_lib.f magnnum.f
	$(FC) $(LDFLAGS) --shared -fPIC -o $@ $+  $(LDLIBS)

octave: libmagneetti.so
	rm -f *.mex
	octave -q --eval 'build_mex'

clean:
	rm -f magnet magnet_new libmagneetti.so *.o