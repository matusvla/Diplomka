# script runprog.py
# (c) Vladislav Matus
#  last edit: 10. 11. 2018

import os
import sys
from subprocess import call, Popen, PIPE

MATRIX_PATH = "matrices"
OUTPUT_FILE = "progoutput.txt"
DELIMITER = "-------------------------------------------------------"

scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
call(["make"])  # TODO delete
try:
    os.remove(OUTPUT_FILE)
except OSError:
    pass

# for matrix in sorted(os.listdir(MATRIX_PATH)):
#     print("Processing " + matrix)
#     command = "./prog -mt P3 -o ./" + MATRIX_PATH + \
#         "/" + matrix + " >> progoutput.txt"
#     child = Popen(command, shell=True, stdout=PIPE)
#     print(child.communicate()[0] + DELIMITER)

for matrix in range(3, 50):
    print("Processing P" + str(matrix))
    command = "./prog -mt P"+str(matrix)+" >> progoutput.txt"
    child = Popen(command, shell=True, stdout=PIPE)
    print(child.communicate()[0] + DELIMITER)
