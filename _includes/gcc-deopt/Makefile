NASM := /usr/local/bin/nasm
NASMFLAGS = -fmacho64
CC := gcc-5
CFLAGS := -W -Wall -Ofast -m64 -march=native -mtune=native -fomit-frame-pointer

TARGETS := test-offset.o offset.o test-offset

%.o: %.asm
	$(NASM) $(NASMFLAGS) $< -o$@

check: all
	./test-offset

all: $(TARGETS)

test-offset: test-offset.o offset.o

clean:
	rm -f $(TARGETS)
