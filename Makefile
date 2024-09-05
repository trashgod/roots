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
	pushd $(LIBDIR) && $(GPRBUILD) && popd

release:
	$(GPRBUILD) -Xmode=release
	pushd $(LIBDIR) && $(GPRBUILD) -Xmode=release && popd

run:
	./hash/examples.sh
	./roots/examples.sh | grep error
	./stream/examples.sh
	./war/examples.sh
	./words/examples.sh
	pushd $(LIBDIR) && ./examples.sh && popd

clean:
	$(GPRCLEAN)
	$(GPRCLEAN) -r -P $(LIBDIR)/default

cleaner: clean
	$(RMDIR) $(OBJDIR)
	$(RMDIR) $(LIBDIR)/$(OBJDIR) $(LIBDIR)/lib                                                                            