# Very very very simple python output format checkcer - part of FLP Project 1 2023
# Python script to test regexes because I dont have the patience to do it in bash
# Author: Vojtech Fiala

import sys
import re

inp = ""
for line in sys.stdin:
    inp += line

def validKey(x):
    res = re.search("\s*Key\s*\{\s*d\s*:\s*0x([a-f]|[0-9])+\s*Q\s*:\s*0x([a-f]|[0-9])+\s*\}", x)
    if not res:
        return 0
    else:
        return 42

def validSig(x):
    res = re.search("\s*Signature\s*\{\s*r\s*:\s*0x([a-f]|[0-9])+\s*s\s*:\s*0x([a-f]|[0-9])+\s*\}", x)
    if not res:
        return 0
    else:
        return 43

def valid(x):
    res = re.search("True", x)
    if not res:
        return 0
    else:
        return 44

def invalid(x):
    res = re.search("False", x)
    if not res:
        return 0
    else:
        return 45

def exitWith(x):
    exit(x)

switch = sys.argv[1]
if switch == '-k':
    exitWith(validKey(inp))
elif switch == '-s':
    exitWith(validSig(inp))
elif switch == '-vT':
    exitWith(valid(inp))
elif switch == '-vF':
    exitWith(invalid(inp))

exit(1)
