# MAKEFILE for FLP Project 1 2023
# Author: Vojtech Fiala

CC = ghc
TARGET = flp22-fun
CFLAGS = --make -Wall

.PHONY: all clean 

all:
	$(CC) $(CFLAGS) $(TARGET).hs -o ../$(TARGET);
# $(MAKE) clean;

clean:
	rm ./*.hi
	rm ./*.o
