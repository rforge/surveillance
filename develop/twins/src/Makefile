#Provide path to the Gnu Scientific Library (GSL) available from 
#http://sources.redhat.com/gsl/

#GSL_PATH
GSL_PATH=/opt/local/include/ 

#LIBPATH
LIBPATH=/opt/local/lib
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

multi2: 
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimateI3.exe estimateI3-vbdichte-version.cc -lgsl 

multi3: 
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimateI3.exe estimateI3-logit-version7.cc -lgsl 

multi4: 
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimateI3.exe estimateI3-logit-version-Z1.cc -lgsl 

multi5: 
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimateI3.exe estimateI3-logit-version-Z1-ga-la.cc -lgsl 

fb:
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimate-m2-fb.exe estimate-m2-fb.cc -lgsl 

fb2:
	${CC} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimate-m2-fb.exe estimate-m2-fb-sk3.cc -lgsl 

smc: acm582.o fortran.o estimate-smc.o 
	${LD} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimate-smc.exe estimate-smc.o fortran.o acm582.o -lm -lgsl -lg2c 

estimate-smc.o : estimate-smc.cc
	$(CC) -c -I${GSL_PATH} $(CCCFLAGS) -o estimate-smc.o estimate-smc.cc 

m: acm582.o fortran.o estimate-multi.o 
	${LD} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimate-multi estimate-multi.o fortran.o acm582.o -lm -lgsl -lgfortran -lcblas #-lg2c 

estimate-multi.o : estimate-multi.cc
#	$(CC) -c -I${GSL_PATH} $(CCCFLAGS) -o estimate-multi.o estimate-multi.cc 
	$(CC) -c -I${GSL_PATH} $(FLAGS) -o estimate-multi.o estimate-multi.cc 

mtemp: acm582.o fortran.o estimate-multi-temp.o 
	${LD} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} -o estimate-multi-temp.exe estimate-multi-temp.o fortran.o acm582.o -lm -lgsl -lg2c 

estimate-multi-temp.o : estimate-multi-temp.cc
	$(CC) -c -I${GSL_PATH} $(CCCFLAGS) -o estimate-multi-temp.o estimate-multi-temp.cc 

multi: acm582.o fortran.o estimateI3.o 
	${LD} -I${GSL_PATH} -L${LIBPATH}  ${FLAGS} ${PFLAGS} -o estimateI3.exe estimateI3.o fortran.o acm582.o -lm -lgsl -lg2c -pg

estimateI3.o : estimateI3.cc
	$(CC) -c -I${GSL_PATH} $(CCCFLAGS) ${PFLAGS} -o estimateI3.o estimateI3.cc -pg

fortran.o : fortran.f
	$(FC) -c $(CCCFLAGS) -o fortran.o fortran.f 

acm582.o : acm582.f
	$(FC) -c $(CCCFLAGS) -o acm582.o acm582.f

estim:
	estimate  ${DATA}/hepatitisA.txt  ${DATA}/hepatitisA-xi.txt ../output/hepatitisA.log ../output/hepatitisA2.log 1000 10 1000 1000 1 0.25 lambda 1 1 nu 1 1 psi 1 0.1 1 1 1 1 1 1 1 0.000001 52 1 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 0.1 1 1

profile:
	gcov -b -f estimateI3.cc

profile2:
	gprof estimateI3.exe gmon.out >profile.txt


