FC      = gfortran
#EJH# FCFLAGS = -O0 -g -fcheck=all -fno-realloc-lhs -Wall -std=f2003
FCFLAGS = -O0 -g -fcheck=all -fno-realloc-lhs -Wall

all: re_example re_test shakespeare_test

re_example: re_example.o regex.o

re_example.o: regex.o

re_test: re_test.o regex.o

shakespeare_test: shakespeare_test.o regex.o

re_test.o: regex.o 

%: %.o
	$(FC) -o $@ $^ $(FLFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

test: re_test
	./re_test
	./shakespeare_test

clean:
	rm -rf *.o *.mod re_test re_example shakespeare_test

again:
	$(MAKE) clean
	$(MAKE) all
