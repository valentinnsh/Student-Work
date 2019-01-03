SOURCES = knight.f90
OBJECTS = knight.o
FINPROGNAME = knight
FLAGS = -g -c
all:  $(OBJECTS) $(FINPROGNAME) clean run

knight: $(OBJECTS)
	gfortran $(OBJECTS) -o $(FINPROGNAME)

knight.o: knight.f90
	gfortran $(FLAGS) knight.f90
run:
	./$(FINPROGNAME) > result
	gedit --new-window result
clean: 
	rm -rf *.o

	
