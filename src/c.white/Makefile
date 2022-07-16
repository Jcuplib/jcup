TOPDIR = ../../../../../..
include ${TOPDIR}/src/Mkinclude
include files.mk

include files.mk

JALMODS = $(JALOBJS:.o=.mod)


all:
	$(MAKE) modules
	@echo "Complete making $(LIBJCUP)."

modules: $(JALOBJS)
	$(INSTALL) $(JALOBJS) ../
	$(INSTALL) $(JALMODS) ../

clean:
	$(RM) -f *.o *.mod *.lst *.L

.SUFFIXES:
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(filter-out -heap-arrays, $(FFLAGS)) -c $< -I../c.black 

%.mod: %.f90
	make $(patsubst %.f90,%.o,$<)
