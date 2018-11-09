import os
from subprocess import call
scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
call(["./prog","-mt","T1","-o","./matrices/bcsstk02.rsa"])