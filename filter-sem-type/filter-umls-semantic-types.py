#import sys
#from os import listdir, mkdir
from os.path import isfile, join, isdir
from collections import defaultdict 


import sys, getopt


PROG_NAME = "filter-umls-semantic-types.py"


def usage(out):
    print("Usage: "+PROG_NAME+" [options] <SemGroups.txt> <UMLS dir> <types>",file=out)
    print("",file=out)
    print("  Uses UMLS files to filter a list of semantic types:",file=out)
    print("    <SemGroups.txt> is obtained from https://lhncbc.nlm.nih.gov/semanticnetwork/download/SemGroups.txt",file=out)
    print("    <UMLS dir> contains META/MRSTY.RRF and META/MRCONSO.RRF",file=out)
    print("    <types> is a comma-separated list of coarse types e.g. 'DISO,GENE' (see first col in SemGroups.txt)",file=out)
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

if len(args) != 3:
    usage(sys.stderr)
    sys.exit(2)

semGroupsFile = args[0]
umlsDir= args[1]
targetCoarseTypes = args[2].split(",")


targetTypes = []
with open(semGroupsFile) as infile:
    for line in infile:
        cols = line.rstrip().split("|")
        if cols[0] in targetCoarseTypes:
            targetTypes.append(cols[2])

print("info: identified %d target types" % (len(targetTypes)))

targetCuis = set()
with open(join(umlsDir,"META","MRSTY.RRF")) as infile:
    for line in infile:
        cols = line.rstrip().split("|")
        if cols[1] in targetTypes:
            targetCuis.add(cols[0])

print("info: identified %d target cuis" % (len(targetCuis)))

for cui in targetCuis:
    print(cui)

