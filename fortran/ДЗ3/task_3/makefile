files=$(wildcard *.f90)
obj=$(patsubst %.f90, %.o, $(files))

res: result

result:  main
	./main > result
main: $(obj)                                                          
	gfortran $^ -o $@
%.o: %.f90
	gfortran -c $^ -o $@
rule2: main
	 ./main
clean:
	rm -f *.o *.mod *.mod0
