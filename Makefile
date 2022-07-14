BINARY=rayforth
OBJECT=main.o
ASM=nasm
LD=ld
#ASMFLAGS=-f elf -g -F dwarf
#LDFLAGS=-m elf_i386 -g --omagic
ASMFLAGS=-f elf
LDFLAGS=-m elf_i386 --omagic

.PHONY: clean

all: $(BINARY)

%.o : %.asm

$(BINARY):
	${ASM} ${ASMFLAGS} main.asm
	${LD} ${LDFLAGS} -s ${OBJECT} -o ${BINARY}

clean:
	rm -f *.o ${BINARY} nul
