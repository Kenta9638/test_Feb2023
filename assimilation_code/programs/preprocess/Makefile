# Makefile created by mkmf $Id$ 

include /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/build_templates/mkmf.template
.DEFAULT:
	-touch $@
all: fixsys /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/programs/preprocess/preprocess
fixsys: 
	@ (cd /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities; ./fixsystem $(FC) )
utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90 types_mod.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90
preprocess.o: preprocess.f90 types_mod.o utilities_mod.o parse_args_mod.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	preprocess.f90
time_manager_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90 types_mod.o utilities_mod.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90
parse_args_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90 utilities_mod.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90
types_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90
null_mpi_utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
./utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90 .
./types_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90 .
./parse_args_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90 .
./null_mpi_utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90 .
./time_manager_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90 .
SRC = /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90 preprocess.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90
OBJ = utilities_mod.o preprocess.o time_manager_mod.o parse_args_mod.o types_mod.o null_mpi_utilities_mod.o
OFF = /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/types_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/parse_args_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/null_mpi_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/modules/utilities/time_manager_mod.f90
clean: neat
	-rm -f .cppdefs $(OBJ) /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/programs/preprocess/preprocess
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/programs/preprocess/preprocess: $(OBJ)
	$(LD) $(OBJ) -o /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART/assimilation_code/programs/preprocess/preprocess  $(LDFLAGS)
