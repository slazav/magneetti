prefix     ?= /usr
bindir     ?= ${prefix}/bin
libdir     ?= ${prefix}/lib
python3_sitelibdir ?= ${libdir}/python3/site-packages

#  -g -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal\
FFLAGS= -g -Wconversion\
  -ffpe-summary=none\
  -Wline-truncation\
  -Waliasing  -Wampersand -Warray-bounds -Wcharacter-truncation\
  -Wline-truncation -Wno-tabs -Wunderflow\
  -Wno-unused-parameter -fPIC -fno-range-check -O\
  -std=legacy

all: magnet magnet_new libmagnet.so

# original magnet program
magnet: magnet.f magnet_lib.f magnnum.f

# library
libmagnet.so: magnet_lib.f magnnum.f
	$(FC) $(LDFLAGS) --shared -fPIC -o $@ $+  $(LDLIBS)

# new interface
magnet_new.o: magnet.h magnet_new.cpp
magnet_new: magnet_new.o magnet_lib.o magnnum.o
	g++ -O9 -o magnet_new $+ -lgfortran

# octave interface
octave: libmagnet.so
	rm -f *.mex
	octave-cli -q --eval 'build_mex'

clean:
	rm -f magnet magnet_new *.so *.o *.mex
	make -C example_new  clean
	make -C example_orig clean
	make -C example_py   clean

install: all
	install -D -m755 magnet_new magnet_plot -t ${bindir}/
	install -D -m644 magnet.py  -t ${python3_sitelibdir}/magnet/
	echo "from .magnet import *" > ${python3_sitelibdir}/magnet/__init__.py
