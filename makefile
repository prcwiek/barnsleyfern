.SUFFIXES:

FC = gfortran

# compile flags
FCFLAGS = -ffree-line-length-none $$(pkg-config --cflags --libs gtk-3-fortran)

barnsley_fern:	barnsley_fern.f90
		$(FC) $< $(FCFLAGS) -o $@.out

%.mod: %.f90
		$(FC) $(FCFLAGS) -c $<	


PHONY: clean
clean:
	-rm -f *.o *.mod *.smod *.anc barnsley_fern
