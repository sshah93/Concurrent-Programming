NAME = primes
SRC = primes.c
OBJ = $(SRC:.c=.o)
CFLAGS = -Wall
CERRORS = -pedantic-errors
RM = rm -f

all: $(OBJ)
	$(GCC) $(CFLAGS) $(CERRORS) $(SRC) -o $(NAME)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) \#*
	$(RM) .*.swp
	$(RM) *.core

fclean: clean
	$(RM) $(NAME)

re: fclean all