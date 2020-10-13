### Magnet program

This program calculates magnetic fields produced by a system of
cocentric coils in a superconducting shields. The shield must consist
of cocentric cylinders and circular disks. There can be a hole in the
center of each disk.

* `magnet` -- original program with Q-and-A interface
-- Michael Boegl, EPV, Uni-Bayreuth, 1993

*`magneettiUI` -- tcl/tk interface (answer questions in GUI)
-- Jaakko Ruohio <jaakko.ruohio@hut.fi>, 1999

* `magneettiSH` -- read human-readable magnet description and produce
 answers for `magnet` program (see `example_orig/` folder)
-- Vladislav Zavjalov <slazav@altlinux.org>, 2014

* `magnet_new`  --  new command-line interface similar to `magneettiSH +
magnet`, see (see `example_new/` folder)
-- Vladislav Zavjalov, 2016-2020

* `magnet_plot` -- make gnuplot script from human-readable magnet
description (see `example_new/` or `example_orig/` folder)
 -- Vladislav Zavjalov <slazav@altlinux.org>, 2020

* `magnet.py` -- python interface (see `example_py/` folder)

* `magnet.mex` -- octave/matlab interface. Not finished.


### KNOWN PROBLEMS:

- Calculation grid should not match with shield ends.
Sometimes calculation just stops there.

