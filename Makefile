BINARY=rayforth
OBJECT=main.o
ASM=nasm
LD=ld
ASMFLAGS=-f elf64
LDFLAGS=-m elf_x86_64 -s --omagic
ASMFLAGSDBG=-f elf64 -g -F dwarf
LDFLAGSDBG=-m elf_x86_64 -g --omagic

.PHONY: clean

all: $(BINARY)

%.o : %.asm

$(BINARY): main.asm
	${ASM} ${ASMFLAGS} main.asm
	${LD} ${LDFLAGS} ${OBJECT} -o ${BINARY}

debug: main.asm
	${ASM} ${ASMFLAGSDBG} main.asm
	${LD} ${LDFLAGSDBG} ${OBJECT} -o ${BINARY}

clean:
	rm -f *.o ${BINARY} nul
