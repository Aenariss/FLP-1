CC = ghc
TARGET = flp22-fun
CFLAGS = --make -Wall

.PHONY: all clean

all:
	$(CC) $(CFLAGS) $(TARGET).hs -o $(TARGET)
	$(MAKE) clean

clean:
	rm ./*.hi
	rm ./*.o