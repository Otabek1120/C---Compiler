CC = gcc
CFLAGS = -Wall -g

# Object files
OBJS = driver.o parser.o scanner.o ast-print.o code_gen.o

# Compile each source file to an object file
driver.o: driver.c
	$(CC) $(CFLAGS) -c driver.c

parser.o: parser.c
	$(CC) $(CFLAGS) -c parser.c

scanner.o: scanner.c scanner.h
	$(CC) $(CFLAGS) -c scanner.c

ast-print.o: ast-print.c ast.h
	$(CC) $(CFLAGS) -c ast-print.c

code_gen.o: code_gen.c code_gen.h ast.h
	$(CC) $(CFLAGS) -c code_gen.c

# Link object files into a binary
compile: $(OBJS)
	$(CC) $(CFLAGS) -o compile $(OBJS)

clean:
	rm -f *.o compile