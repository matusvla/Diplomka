# Makefile
# (c) Vladislav Matus
# last edit: 28. 09. 2018
# Variable initializations
FC = gfortran
FFLAGS = -c -O2 -J./modules -I./modules
CC = gcc
CFLAGS = -c -O2

OBJECTS= \
	raggedmultiarray.o mrgrnk.o gvroutines.o auxroutines.o \
	mydepend.o mydepend90.o myroutines90.o cmdlineloader.o\
	testing.o svrev.o matrixloader.o \
	metis_interface.o metisinclude.o \
	  main.o
LIBMETIS = /usr/local/lib/libmetis.a

OBJDIR = objects
MODDIR = modules

# Prefixing OBJECTS with OBJDIR to create folder structure
OBJ = $(addprefix $(OBJDIR)/,$(OBJECTS))
     
# Building the project		 
main: ${OBJ}
	$(FC) -o prog $(OBJ) $(LIBMETIS) 

$(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) -o $@ $^

$(OBJDIR)/%.o: %.f
	$(FC) $(FFLAGS) -o $@ $^

$(OBJDIR)/%.o: %.F
	$(FC) $(FFLAGS) -o $@ $^

$(OBJDIR)/%.o: %.f90
	$(FC) $(FFLAGS) -o $@ $^   

$(OBJDIR)/%.o: %.f95
	$(FC) $(FFLAGS) -o $@ $^
     
# Clean target
clean :
	rm -f  *.o ./${MODDIR}/*.mod ./${OBJDIR}/*.o prog.exe
