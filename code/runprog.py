# script runprog.py
# (c) Vladislav Matus
#  last edit: 10. 11. 2018

import os
import sys
from subprocess import call, Popen, PIPE

MATRIX_PATH = "testmatrices"
OUTPUT_FILE = "progoutput.txt"
DELIMITER = "-------------------------------------------------------"

scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
call(["make"])  # TODO delete
try:
    os.remove(OUTPUT_FILE)
except OSError:
    pass

for matrix in os.listdir(MATRIX_PATH):
    print("Processing " + matrix)
    command = "./prog -o ./" + MATRIX_PATH + "/" + matrix + " -t >> progoutput.txt"
    child = Popen(command, shell=True, stdout=PIPE)
    print child.communicate()[0] + DELIMITER
