# script runprog.py
# (c) Vladislav Matus
#  last edit: 10. 11. 2018

import os
import sys
import re
from subprocess import call, Popen, PIPE

MATRIX_PATH = "matrices"
OUTPUT_FILE = "progoutput.txt"
DELIMITER = "-------------------------------------------------------"

scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
try:
    os.remove(OUTPUT_FILE)
except OSError:
    pass

for matrix in sorted(os.listdir(MATRIX_PATH)):
    otValues = ['no','DIST','MD','MIX']
    for otValue in otValues:
        # print("Processing " + matrix)
        command = "./prog -f ./" + MATRIX_PATH + \
            "/" + matrix + " " + \
            "-ot " + otValue
        proc = Popen(command, shell=True, stdout=PIPE)
        (out,err) = proc.communicate()
        # print(out)
        outValues = map(int, re.findall(' \d+', out))
        if len(outValues) == 0 :
            print(matrix.replace("_", "\_") + " \t&\tERROR \t&\tERROR \t&\tERROR \t\\\\")
        else:
            if(otValue == 'no'):
                print(matrix.replace("_", "\_") + '\t&\t' + \
                '\t&\t'.join(map(str,outValues)) + "\t\\\\")
            else:
                print('\t&\t' + \
                '\t&\t'.join(map(str,outValues)) + "\t\\\\")

# for size in range(3, 50):
#     # print("Processing P" + str(matrix))
#     command = "./prog -mt P"+str(size)+ \
#         ""
#     proc = Popen(command, shell=True, stdout=PIPE)
#     (out,err) = proc.communicate()
#     outValues = map(int, re.findall(' \d+', out))
#     if len(outValues) == 0 :
#         print('P'+ size + " \t&\tERROR \t&\tERROR \t&\tERROR \t//")
#     else:
#         print('P'+ str(size) + '\t&\t' + '\t&\t'.join(map(str,outValues)) + "\t\\\\")

# for matrix in range(1, 8):
#     print("Processing T" + str(matrix))
#     command = "./prog -mt T"+str(matrix)+" -t >> progoutput.txt"
#     child = Popen(command, shell=True, stdout=PIPE)
#     print(child.communicate()[0] + DELIMITER)
