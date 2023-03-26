# MAKEFILE for FLP Project 1 2023
# Author: Vojtech Fiala

SRCDIR = ./src
TESTFILE = tests/tests-launch

.PHONY: all test pack

all:
	$(MAKE) -C $(SRCDIR) all

test:
	chmod +x ./$(TESTFILE)
	./$(TESTFILE)

pack:
	zip -r flp-fun-xfiala61.zip Makefile tests src doc