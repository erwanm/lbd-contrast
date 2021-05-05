#import sys
#from os import listdir, mkdir
from os.path import isfile, join, isdir
from collections import defaultdict 


import sys, getopt


PROG_NAME = "filter-column.py"


def usage(out):
    print("Usage: cat <patterns> | "+PROG_NAME+" [options] <input file> <col no>",file=out)
    print("",file=out)
    print("  Filters <input file> keeping only line where column <col no> contains a value which ",file=out)
    print("  belongs to one of the <patterns> given on STDIN.",file=out)
    print("",file=out)
    print("  Options")
    print("    -h: print this help message.",file=out)
    print("",file=out)


# main

input_multi_concept = False
try:
    opts, args = getopt.getopt(sys.argv[1:],"h")
except getopt.GetoptError:
    usage(sys.stderr)
    sys.exit(2)
for opt, arg in opts:
    if opt == '-h':
        usage(sys.stdout)
        sys.exit()

# print("debug args after options: ",args)

if len(args) != 2:
    usage(sys.stderr)
    sys.exit(2)

inputFile=args[0]
s=args[1]
colNo=int(args[1])-1

patterns=set()
print("Reading patterns on STDIN...",file=sys.stderr)
for line in sys.stdin:
    patterns.add(line.rstrip())


print("Filtering input file...",file=sys.stderr)
with open(inputFile) as infile:
    for line in infile:
        cols = line.rstrip().split("\t")
        if cols[colNo] in patterns:
            print(line,end='')

