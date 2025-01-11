# ComET
Composite Engineering Toolbox (ComET)

# Quick installation guide (Linux)

Clone the repo:

```bash
git clone https://github.com/mnolaya/ComET.git
```

Install gfortran and make:

```bash
sudo apt-get install gfortran make
```

Build the shared libraries using the Makefile:

```bash
cd comet
make
```

Shared libraries (`*.so`) to be linked to a main program are found in [./comet/bin](./comet/bin) and module files in [./comet/include](./comet/include). These should be compiled along with the main program source file. For example:

```bash
gfortran -ffree-line-length-none -I../comet/include -o output_file_name some_main_file.f90 -L../comet/bin -Wl,-rpath,../comet/bin -lglobals -lelement_library
```

Then to run:

```bash
chmod +x output_file_name
./output_file_name
```

Refer to the [tests](./tests) directory for a [bash script](./tests/run_test) containing example bash commands streamlining the build/compilation/execution process of a [test program](./tests/test_2d_element.f90) that uses the ComET library.