# Computer Systems Engineering, ITMO University, First Year

This is my lab work & assignment solutions repository for the first year
of studies for Bachelor's in Computer Science and Engineering at ITMO
University.

If you find any of the notes or sources useful, you are free to copy and
modify them. Do note, however, that the code was written for educational
purposes only and may well be naive and unoptimized.

## Structure

The repository layout has grown organically and doesn't really follow
any kind of logic. While I do regret not paying more attention to this,
I do not want to reorganize it now, since the study year is already over.

Summarized briefly, here's what you'll see in each directory:

* `csb` -- assignments for the __Computer Science Basics__ course, mostly
featuring _Basic Computer_ assembly, an instruction set reminiscent
of that of [PDP-8](https://en.wikipedia.org/wiki/PDP-8), albeit more limited.
* `db` -- lab and coursework scripts for the __Databases__ course, targeting
PostgreSQL 9.6.
* `inf` -- document processing macros for the __Informatics__ course,
written in Python.
* `misc` -- intended for miscellaneous files, now containing Vim syntax files
and functions for writing _Basic Computer_ assembly.
* `notes` -- IHaskell notebooks with __Discrete Math__ algorithm implementations
for two (maximum flow and shortest path) of the seven assigments we were given;
several more were programmed in
[collaboration with three other students](https://github.com/band-of-four/discrete-math-homework-second-term),
while the rest of the problems were solved by hand.
* `reports` -- lab and coursework reports for all of the aforementioned courses and more,
composed using [HaTeX](http://hackage.haskell.org/package/HaTeX).
* `se` -- projects for the __Programming__ course, done in Kotlin, with a little bit
of Java and Clojure sprinkled on top. As the final assignment, I implemented a
[toy ORM](https://github.com/thymelous/pearl), moved into a separate repository so as
to enable CI and JitPack builds.
