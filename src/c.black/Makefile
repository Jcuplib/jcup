TOPDIR = ../..
include ${TOPDIR}/Mkinclude
include files.mk

CMODS = $(COBJS:.o=.mod)


all:
	$(MAKE) modules
	@echo "Complete making $(LIBJCUP)."

modules: $(COBJS)
	$(INSTALL) $(COBJS) ../
	$(INSTALL) $(CMODS) ../

clean:
	$(RM) -f *.o *.mod *.lst *.L

.SUFFIXES:
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

%.mod: %.f90
	make $(patsubst %.f90,%.o,$<)

