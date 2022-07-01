include c.black/files.mk
include c.white/files.mk

JCUPAPI  = jcup_interface.o

JCUPOBJS = $(COBJS) $(JALOBJS) $(JCUPAPI)
