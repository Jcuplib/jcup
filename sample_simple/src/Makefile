include ./Mkinclude

.SUFFIXES: .f90 .o

OBJS = jcup_interpolation_sample.o common.o 
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

TARGET1 = comp1
TARGET2 = comp2

all : mapping_table ${TARGET1} ${TARGET2}

${TARGET1} : $(OBJS) comp1.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBJCUP)

${TARGET2} : $(OBJS) comp2.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBJCUP)

mapping_table : common.o cal_mappingtable.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBJCUP)

clean:
	-rm -f *mod *o

.f90.o:	
	$(FC) -c $< $(FFLAGS) -I$(INCJCUP) -I$(BINDIR) 
