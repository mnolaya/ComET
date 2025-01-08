ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
SRC=$(ROOT_DIR)fort
BIN=$(ROOT_DIR)bin
MOD=$(ROOT_DIR)include
BUILD=$(ROOT_DIR)build

FC=gfortran
FCFLAGS=-fPIC -ffree-line-length-none
LNKFLAGS=-fPIC -fno-underscoring -shared -Wl,-rpath=`pwd`

all: make_dirs globals element_library copy clean

make_dirs:
	mkdir -p $(BUILD)
	mkdir -p $(BIN)
	mkdir -p $(MOD)

globals:
	$(FC) $(FCFLAGS) -c $(SRC)/constants.f90 -I$(BUILD) -J$(BUILD)
	$(FC) -shared -o $(BUILD)/libglobals.so *.o
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