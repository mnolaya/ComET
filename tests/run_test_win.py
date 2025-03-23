import subprocess

EXE = 'run_test_win.exe'
compile_args = [
    'ifx', # Compiler
    'test_main.f90', # Source file
    r'C:\Users\micha\repos\ComET\comet\win_build\globals\x64\Debug\*.lib', # Library
    r'C:\Users\micha\repos\ComET\comet\win_build\linear_algebra\x64\Debug\*.lib', # Library
    r'C:\Users\micha\repos\ComET\comet\win_build\element_library\x64\Debug\*.lib', # Library
    '-I',
    r'C:\Users\micha\repos\ComET\comet\win_build\globals\x64\Debug', # Module files to be included
    '-I',
    r'C:\Users\micha\repos\ComET\comet\win_build\linear_algebra\x64\Debug', # Module files to be included
    '-I',
    r'C:\Users\micha\repos\ComET\comet\win_build\element_library\x64\Debug', # Module files to be included
    '/MDd', # Multi-threaded debug
    '/Qmkl',
    '-o',
    EXE
]
exec_args = [
    EXE,
]
p = subprocess.run(args=compile_args)
p = subprocess.run(args=exec_args)