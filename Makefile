BINARY=rayforth
OBJECT=main.o
ASM=nasm
LD=ld
ASMFLAGS=-f elf64 -g -F dwarf
LDFLAGS=-m elf_x86_64 -g --omagic
#ASMFLAGS=-f elf -g -F dwarf
#LDFLAGS=-m elf_i386 -g --omagic
#ASMFLAGS=-f elf
#LDFLAGS=-m elf_i386 -s --omagic

.PHONY: clean

all: $(BINARY)

%.o : %.asm

$(BINARY): main.asm
	${ASM} ${ASMFLAGS} main.asm
	${LD} ${LDFLAGS} ${OBJECT} -o ${BINARY}

clean:
	rm -f *.o ${BINARY} nul
