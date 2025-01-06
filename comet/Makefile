SRC=./fort
BIN=./bin
MOD=./include
BUILD=./build

FC=gfortran
FCFLAGS=

all: make_dirs globals element_library copy clean

make_dirs:
	mkdir -p $(BUILD)
	mkdir -p $(BIN)
	mkdir -p $(MOD)

globals:
	$(FC) $(FCFLAGS) -c $(SRC)/constants.f90 -I $(BUILD) -J $(BUILD) -L $(BUILD)
	$(FC) *.o -shared -o $(BUILD)/libglobals.so
	rm *.o

element_library:
	$(FC) $(FCFLAGS) -c $(SRC)/element_library.f90 -I $(BUILD) -J $(BUILD) -L $(BUILD)
	$(FC) *.o -shared -o $(BUILD)/libelement_library.so
	rm *.o	

copy:
	cp $(BUILD)/*.so $(BIN)
	cp $(BUILD)/*.mod $(MOD)

clean:
	rm -rf $(BUILD)