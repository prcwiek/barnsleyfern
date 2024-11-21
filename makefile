
FC = gfortran
OBJS = gtk-hl-chooser_modified.o

.SUFFIXES: .f90 .o

# compile flags
FCFLAGS = -ffree-line-length-none $$(pkg-config --cflags --libs gtk-3-fortran)

barnsley_fern:	barnsley_fern.f90 $(OBJS)
		$(FC) $< $(OBJS) $(FCFLAGS)  -o $@.out

.f90.o:
		$(FC) $(FCFLAGS) -c $<	

%.o: %.mod

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod *.anc barnsley_fern
