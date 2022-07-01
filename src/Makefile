TOPDIR = ../
include ${TOPDIR}Mkinclude

LIBJCUP = libjcup.a

include files.mk

VPATH = c.black:c.white

MODS = $(JCUPOBJS:.o=.mod)

all: makedir
	$(MAKE) $(JCUPLIBDIR)/$(LIBJCUP)
	$(MAKE) modules
	@echo "Complete making $(LIBJCUP)."

makedir:
	mkdir -p $(JCUPLIBDIR)
	mkdir -p $(JCUPINCDIR)

$(JCUPLIBDIR)/$(LIBJCUP): $(JCUPOBJS)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

modules: $(JCUPOBJS)
	$(INSTALL) $(MODS) $(JCUPINCDIR)


allclean: clean
	$(RM) -f $(LIBDIR)/$(LIBJCUP)

clean:
	$(RM) -f *.o *.mod *.lst *.L



.SUFFIXES:
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

%.mod: %.f90
	make $(patsubst %.f90,%.o,$<)

