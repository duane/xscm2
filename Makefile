all: test-elf.o test-macho.o

test-elf.o:
	scheme --program elf/elf-write.ss

test-macho.o:
	scheme --program macho/macho-write.ss
