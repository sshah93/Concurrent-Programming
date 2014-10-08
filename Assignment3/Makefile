NAME = primes
SRC = primes.c
OBJ = $(SRC:.c=.o)

CERRORS = -pedantic-errors
RM = rm -f

all: $(OBJ)
	gcc -Wall $(CERRORS) $(SRC) -o $(NAME)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) \#*
	$(RM) .*.swp
	$(RM) *.core

fclean: clean
	$(RM) $(NAME)

re: fclean all
