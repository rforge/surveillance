#Provide path to the Gnu Scientific Library (GSL) available from 
#http://sources.redhat.com/gsl/

#GSL_PATH
R_INCLUDE_DIR = /Library/Frameworks/R.framework/Headers
GSL_PATH=/opt/local/include/ 
INCLUDE_DIR = ${R_INCLUDE_DIR}
#${GSL_PATH}:${R_INCLUDE_DIR}

#LIBPATH
LIBPATH=/opt/local/lib
R_LIB_PATH = /Library/Frameworks/R.framework/Libraries
#${GSL_PATH}:${LIB_PATH}  #/.libs

#Define C++ compiler 
CC=g++
FC = gfortran #g77
LD = g++

#Flags for compilation
FLAGS=-Wno-deprecated   
PFLAGS=-ftest-coverage -fprofile-arcs -fbranch-probabilities 

#Flags for compilation
#FLAGS=-Wno-deprecated -O3
#
#LIBPATH=/usr/local/lib/


#Data path
DATA=../data

m: acm582.o fortran.o estimate-multi.o 
	${LD} -I${INCLUDE_DIR} -L${LIBPATH} -L${R_LIB_PATH}  ${FLAGS} -o estimate-multi estimate-multi.o fortran.o acm582.o -lm -lgsl -lgfortran -lcblas -lR #-lg2c 

estimate-multi.o : estimate-multi.cc
	$(CC) -c -I${INCLUDE_DIR} $(FLAGS) -o estimate-multi.o estimate-multi.cc 

fortran.o : fortran.f
	$(FC) -c $(CCCFLAGS) -o fortran.o fortran.f 

acm582.o : acm582.f
	$(FC) -c $(CCCFLAGS) -o acm582.o acm582.f

#estim:
#	./estimate-multi  ${DATA}/hepatitisA.txt  ${DATA}/hepatitisA-xi.txt ../output/hepatitisA.log ../output/hepatitisA2.log 1000 10 1000 1000 1 0.25 lambda 1 1 nu 1 1 psi 1 0.1 1 1 1 1 1 1 1 0.000001 52 1 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 0.1 1 1

estim:
	./estimate-multi twins.ini

profile:
	gcov -b -f estimateI3.cc

profile2:
	gprof estimateI3.exe gmon.out >profile.txt

clean:
	rm *.o estimate-multi

shlib:
#	R CMD SHLIB -c twins.cc twins_main.cc acm582.f fortran.f 
#	R CMD SHLIB -c -o twins.so twins2.cc twins_main.cc acm582.f fortran.f 
#	R CMD SHLIB -c twins.cc acm582.f fortran.f 
	R CMD SHLIB -c twins.cc acm582.f fortran.f 
#	R CMD SHLIB -c twins2.cc
#	R CMD SHLIB -c twins3.c
