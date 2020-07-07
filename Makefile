all: test-out.o

test-out.o:
	chez --program src/elf/elf-write.ss
