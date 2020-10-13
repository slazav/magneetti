#  -g -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal\
FFLAGS= -g -Wconversion\
  -ffpe-summary=none\
  -Wline-truncation\
  -Waliasing  -Wampersand -Warray-bounds -Wcharacter-truncation\
  -Wline-truncation -Wno-tabs -Wunderflow\
  -Wno-unused-parameter -fPIC -fno-range-check -O\
  -std=legacy

all: magnet magnet_new libmagneetti.so

# original magnet program
magnet: magnet.f magnet_lib.f magnnum.f

# library
libmagneetti.so: magnet_lib.f magnnum.f
	$(FC) $(LDFLAGS) --shared -fPIC -o $@ $+  $(LDLIBS)

# new interface
magnet_new.o: magnet.h magnet_new.cpp
magnet_new: magnet_new.o magnet_lib.o magnnum.o
	g++ -O9 -o magnet_new $+ -lgfortran

# octave interface
octave: libmagneetti.so
	rm -f *.mex
	octave-cli -q --eval 'build_mex'

clean:
	rm -f magnet magnet_new libmagneetti.so *.o


install: all
	mkdir -p ${bindir}
	install -m755 magnet_new ${bindir}
