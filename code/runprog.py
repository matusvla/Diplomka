# script runprog.py
# (c) Vladislav Matus
#  last edit: 10. 11. 2018

import os
import sys
import re
from subprocess import call, Popen, PIPE

MATRIX_PATH = "testmatrices"
OUTPUT_FILE = "progoutput.txt"
DELIMITER = "-------------------------------------------------------"

scriptdirpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptdirpath)
try:
    os.remove(OUTPUT_FILE)
except OSError:
    pass


# for matrix in range(200,201,50):
#     output1 = ""
#     output2 = ""
#     for value in  range(0,11,1):
#         # print("Processing " + matrix)
#         command = "./prog -mt P" + str(matrix) + " " + \
#             "-ot MIX" + str(value/10.0)
#         proc = Popen(command, shell=True, stdout=PIPE)
#         (out,err) = proc.communicate()
#         # print(out)
#         outValues = map(int, re.findall(' \d+', out))
#         output1 += "&" + str(outValues[3])
#         output2 += "&" + str(outValues[4])
#     print("\\multirow{2}{*}{P"+ str(matrix) + "}" + output1 + "\t\\\\")
#     print(output2 + "\t\\\\")
#     print("\\hline")


# for matrix in range(200,201,100):
#     otValues = ['no','DIST','MD','MIX']
#     for otValue in otValues:
#         # print("Processing " + matrix)
#         command = "./prog -mt P"+str(matrix)+" -ot " + otValue
#         proc = Popen(command, shell=True, stdout=PIPE)
#         (out,err) = proc.communicate()
#         # print(out)
#         outValues = map(int, re.findall(' \d+', out))
#         if len(outValues) == 0 :
#             print("P"+str(matrix) + " \t&\tERROR \t&\tERROR \t&\tERROR \t\\\\")
#         else:
#             if(otValue == 'no'):
#                 print("P"+str(matrix) + '\t&\t' + \
#                 '\t&\t'.join(map(str,outValues)) + "\t\\\\")
#             else:
#                 print('\t&\t' + \
#                 '\t&\t'.join(map(str,outValues)) + "\t\\\\")


for matrix in sorted(os.listdir(MATRIX_PATH)):
    otValues = ['no','DIST','MD','MIX']
    for otValue in otValues:
        for mvsVal in range(1,10):
            # print("Processing " + matrix)
            command = "./prog -f ./" + MATRIX_PATH + \
                "/" + matrix + " " + \
                "-ot " + otValue + " -oe -mvs " + str(mvsVal)
            proc = Popen(command, shell=True, stdout=PIPE)
            (out,err) = proc.communicate()
            # print(out)
            outValues = map(int, re.findall(' \d+', out))
            if len(outValues) == 0 :
                print(matrix.replace("_", "\_") + " \t&\tERROR \t&\tERROR \t&\tERROR \t\\\\")
            else:
                print(matrix.replace("_", "\_") + " mvs:" + str(mvsVal) + " ot:" + str(otValue))
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
