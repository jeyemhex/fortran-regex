FC      = gfortran
FCFLAGS = -O0 -g -fcheck=all -fno-realloc-lhs -Wall -std=f2003
FLFLAGS = 

all: re_test re_check

re_test: re_test.o regex.o

re_check: re_check.o regex.o

re_test.o: regex.o

re_check.o: regex.o


%: %.o
	$(FC) -o $@ $^ $(FLFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -rf *.o *.mod re_test

again:
	$(MAKE) clean
	$(MAKE) all
