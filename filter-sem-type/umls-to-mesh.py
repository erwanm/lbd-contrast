#import sys
#from os import listdir, mkdir
from os.path import isfile, join, isdir
from collections import defaultdict 


import sys, getopt


PROG_NAME = "umls-to-mesh.py"


def usage(out):
    print("Usage: cat <UMLS CUIs input> | "+PROG_NAME+" [options] <UMLS dir>",file=out)
    print("",file=out)
    print("  Uses UMLS files to convert every CUI read on STDIN to the corresponding Mesh descriptor:",file=out)
    print("    <UMLS CUIs input> is a list of CUIs, one by line.",file=out)
    print("    <UMLS dir> contains META/MRSTY.RRF and META/MRCONSO.RRF",file=out)
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

if len(args) != 1:
    usage(sys.stderr)
    sys.exit(2)

umlsDir= args[0]

print("Reading UMLS...",file=sys.stderr)
m = defaultdict(set)
with open(join(umlsDir,"META","MRCONSO.RRF")) as infile:
    for line in infile:
        cols = line.rstrip().split("|")
        if cols[11] == 'MSH':
            m[cols[0]].add(cols[13])

print("Read %d CUIs with at least 1 corresponding Mesh descriptor" % (len(m)),file=sys.stderr)

res=set()
for line in sys.stdin:
    cui = line.rstrip()
    for mesh in m[cui]:
        res.add(mesh)

for mesh in res:
    print(mesh)
