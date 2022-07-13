BINARY=rayforth
OBJECT=main.o
ASM=nasm
LD=ld
ASMFLAGS=-f elf
LDFLAGS=-m elf_i386

.PHONY: clean

all: $(BINARY)

%.o : %.asm

$(BINARY):
	${ASM} ${ASMFLAGS} main.asm
	${LD} ${LDFLAGS} -s ${OBJECT} -o ${BINARY}

clean:
	rm -f *.o ${BINARY} nul
