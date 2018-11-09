# script runprog.py
# (c) Vladislav Matus
#  last edit: 09. 11. 2018     

import os
from subprocess import call
scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
call(["make"]) #TODO delete
call(["./prog","-mt","RSA","-o","./matrices/bcsstk03.rsa","-t"])