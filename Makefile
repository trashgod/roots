# Makefile for building and running the demonstration prgrams
# License: <a href="https://www.gnu.org/licenses/gpl-3.0.html">GPL</a>

GPRBUILD ?= gprbuild
GPRCLEAN ?= gprclean
OBJDIR   ?= obj
LIBDIR   ?= shared
RMDIR    ?= rmdir

.PHONY: all release run clean cleaner

all:
	$(GPRBUILD)

release:
	$(GPRBUILD) -Xmode=release

run:
	./hash/examples.sh
	./roots/examples.sh | grep error
	./shared/examples.sh
	./stream/examples.sh
	./words/examples.sh

clean:
	$(GPRCLEAN) -r

cleaner: clean
	$(RMDIR) $(OBJDIR) $(LIBDIR)/lib                                                                               