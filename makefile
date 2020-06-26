#format is target-name: target dependencies
#{-tab-}actions

# All Targets
all: root

# Tool invocations
# Executable "hello" depends on the files hello.o and run.o.
root:  new.o
	gcc -g -Wall -o root new.o

# Depends on the source and header files
new.o: new.s
	nasm -g -f elf64 -w+all -o new.o new.s

#tell make that "clean" is not a file name!
.PHONY: clean

#Clean the build directory
clean: 
	rm -f *.o root
