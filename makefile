FC = gfortran
FFLAGS = -c -O2 -J./modules -I./modules

CC = gcc
CFLAGS = -c -O2

OBJ= \
	mydepend.o auxroutines.o mydepend90.o myroutines90.o gvroutines.o raggedmultiarray.o \
	mrgrnk.o svrev.o metis_interface.o metisinclude.o \
	main.o
OBJDIR = objects
LIBMETIS = /usr/local/lib/libmetis.a


%.o: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.o: %.f
	$(FC) $(FFLAGS) -o $@ $<

%.o: %.F
	$(FC) $(FFLAGS) -o $@ $<

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ $<   

%.o: %.f95
	$(FC) $(FFLAGS) -o $@ $<

      
main: ${OBJ}
	$(FC) -o prog ${OBJ} $(LIBMETIS) 
     
#
clean :
	rm -f  *.o ./objects/*.mod prog.exe
