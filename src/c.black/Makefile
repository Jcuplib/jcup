TOPDIR = ../../../../../..
include ${TOPDIR}/src/Mkinclude
include files.mk

CMODS = \
	jcup_buffer.mod  \
	jcup_buffer_base.mod \
	jcup_comp.mod  \
	jcup_config.mod  \
	jcup_constant.mod  \
	jcup_data.mod \
	jcup_data_buffer.mod \
	jcup_exchange.mod \
	jcup_grid.mod \
	jcup_grid_base.mod \
	jcup_grid_checker.mod \
	jcup_intercomm.mod \
	jcup_interpolation.mod \
	jcup_interpolation_interface.mod \
	jcup_io_base.mod \
	jcup_mpi_lib.mod \
	jcup_time.mod \
	jcup_time_buffer.mod \
	jcup_utils.mod \
	jcup_zlib.mod

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
	$(FC) $(filter-out -heap-arrays, $(FFLAGS)) -c $< 

.c.o:
	$(CC) -c $<

%.mod: %.f90
	make $(patsubst %.f90,%.o,$<)

