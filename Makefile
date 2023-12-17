all: test-out.o

test-out.o:
	scheme --program elf/elf-write.ss
