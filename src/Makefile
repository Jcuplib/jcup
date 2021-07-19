TOPDIR = ../
include ${TOPDIR}Mkinclude

LIBJCUP = libjcup.a

include files.mk

BLACK = c.black
WHITE = c.white

MODS = $(JCUPOBJS:.o=.mod)

all: makedir
	cd ${BLACK} ; $(MAKE) ; cd ..
	cd ${WHITE} ; $(MAKE) ; cd ..
	$(MAKE) $(JCUPLIBDIR)/$(LIBJCUP)
	$(MAKE) modules
	@echo "Complete making $(LIBJCUP)."

makedir:
	mkdir -p $(JCUPLIBDIR)
	mkdir -p $(JCUPINCDIR)

$(JCUPLIBDIR)/$(LIBJCUP): $(JCUPAPI)
	$(AR) $(ARFLAGS) $@ $(JCUPOBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

modules: $(JCUPAPI)
	$(INSTALL) $(MODS) $(JCUPINCDIR)


allclean: clean
	$(RM) -f $(LIBDIR)/$(LIBJCUP)

clean:
	cd $(BLACK) ; make clean ; cd ..
	cd $(WHITE) ; make clean ; cd ..
	$(RM) -f *.o *.mod *.lst *.L



.SUFFIXES:
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

%.mod: %.f90
	make $(patsubst %.f90,%.o,$<)

