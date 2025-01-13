ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
SRC=$(ROOT_DIR)fort
BIN=$(ROOT_DIR)bin
MOD=$(ROOT_DIR)include
BUILD=$(ROOT_DIR)build

FC=gfortran
FCFLAGS=-fPIC -ffree-line-length-none
LNKFLAGS=-fPIC -fno-underscoring -shared -Wl,-rpath=`pwd`

LAPACK_LIBDIR=/usr/lib/x86_64-linux-gnu
LAPACK_LIBS=-llapack -lblas

all: make_dirs globals linear_algebra element_library copy clean

make_dirs:
	mkdir -p $(BUILD)
	mkdir -p $(BIN)
	mkdir -p $(MOD)

globals:
	$(FC) $(FCFLAGS) -c $(SRC)/constants.f90 -I$(BUILD) -J$(BUILD)
	$(FC) -shared -o $(BUILD)/libglobals.so *.o
	rm *.o

linear_algebra:
	$(FC) $(FCFLAGS) -c $(SRC)/linear_algebra.f90 -I$(BUILD) -J$(BUILD)
	$(FC) -shared -o $(BUILD)/liblinear_algebra.so *.o -L$(LAPACK_LIBDIR) $(LAPACK_LIBS) -L$(BUILD) -lglobals
	rm *.o


element_library:
	$(FC) $(FCFLAGS) -c $(SRC)/element_utils.f90 -I$(BUILD) -J$(BUILD)
	$(FC) $(FCFLAGS) -c $(SRC)/element_library.f90 -I$(BUILD) -J$(BUILD)
	$(FC) -shared -o $(BUILD)/libelement_library.so  *.o -L$(BUILD) -lglobals
	rm *.o

copy:
	cp $(BUILD)/*.so $(BIN)
	cp $(BUILD)/*.mod $(MOD)

clean:
	rm -rf $(BUILD)