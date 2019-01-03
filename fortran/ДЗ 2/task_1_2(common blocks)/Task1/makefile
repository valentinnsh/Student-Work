files=$(wildcard *.f90)
obj=$(patsubst %.f90, %.o, $(files))

res: result

result: input main
	./main < input > result
main: $(obj)                                                          
	gfortran $^ -o $@
%.o: %.f90
	gfortran -c $^ -o $@
rule2: main
	touch input; ./main
clean:
	rm -f *.o *.mod *.mod0 main
