TOPDIR = ../../../
include ${TOPDIR}Mkinclude

LIBJCUP = libjcup.a

include files.mk

VPATH = c.black:c.white

MODS = $(JCUPOBJS:.o=.mod)

all: 
	#$(MAKE) $(JCUPLIBDIR)/$(LIBJCUP)
	$(MAKE) $(JCUPLIB)
	$(MAKE) modules
	@echo "Complete making $(LIBJCUP)."

makedir:
	mkdir  $(JCUPLIBDIR)
	mkdir  $(JCUPINCDIR)

$(JCUPLIB): $(JCUPOBJS)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

modules: $(JCUPOBJS)
	$(INSTALL) $(MODS) $(MODDIR)


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

