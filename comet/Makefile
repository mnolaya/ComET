ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
SRC=$(ROOT_DIR)fort
BIN=$(ROOT_DIR)bin
MOD=$(ROOT_DIR)include
BUILD=$(ROOT_DIR)build

FC=gfortran
FCFLAGS=-fPIC -ffree-line-length-none
LNKFLAGS=-fPIC -fno-underscoring -shared -Wl,-rpath=`pwd`
REL_LNK=-Wl,-rpath,../comet/bin

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
	$(FC) -shared *.o -L$(LAPACK_LIBDIR) $(LAPACK_LIBS) -L$(BUILD) -o $(BUILD)/liblinear_algebra.so
	rm *.o

element_library:
	$(FC) $(FCFLAGS) -c $(SRC)/element_utils.f90 -I$(BUILD) -J$(BUILD)
	$(FC) $(FCFLAGS) -c $(SRC)/elastic_materials.f90 -I$(BUILD) -J$(BUILD)
	$(FC) $(FCFLAGS) -c $(SRC)/gauss_integration.f90 -I$(BUILD) -J$(BUILD)
	$(FC) $(FCFLAGS) -c $(SRC)/element_library.f90 -I$(BUILD) -J$(BUILD)
	$(FC) -shared *.o -L$(LAPACK_LIBDIR) $(LAPACK_LIBS) -L$(BUILD) -lglobals -llinear_algebra -o $(BUILD)/libelement_library.so
	rm *.o

copy:
	cp $(BUILD)/*.so $(BIN)
	cp $(BUILD)/*.mod $(MOD)

clean:
	rm -rf $(BUILD)