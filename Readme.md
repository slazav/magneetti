### Magnet program

This program calculates magnetic fields produced by a system of
cocentric coils in a superconducting shields. The shield must consist
of cocentric cylinders and circular disks. There can be a hole in the
center of each disk.

* `magnet` -- original program with Q-and-A interface
-- Michael Boegl, EPV, Uni-Bayreuth, 1993

*`magneettiUI` -- tcl/tk interface
-- Jaakko Ruohio <jaakko.ruohio@hut.fi>, 1999

* `magneettiSH` -- read human-readable magnet description and produce
 answers for `magnet` program (see test/ folder)
-- Vladislav Zavjalov <slazav@altlinux.org>, 2014

* `magneetti_plot` -- make gnuplot script from human-readable magnet
description -- Vladislav Zavjalov <slazav@altlinux.org>, 2020

* `magnet_new`  --  new command-line interface similar to `magneettiSH +
magnet` -- Vladislav Zavjalov, 2016-2020


#### KNOWN PROBLEMS:

- Calculation grid should not match with shield ends.
Sometimes calculation just stops there.
