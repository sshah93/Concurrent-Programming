PART1 = rw
PART2 = transfer1
PART3 = transfer2
SRC1 = rw.c
SRC2 = transfer1.c
SRC3 = transfer2.c
OBJ = $(SRC:.c=.o)
CERRORS = -pedantic-errors
LIBS = -lpthread
RM = rm -f

all: $(OBJ)
	gcc -Wall $(CERRORS) $(SRC1) $(LIBS) -o $(PART1)
	gcc -Wall $(CERRORS) $(SRC2) $(LIBS) -o $(PART2)
	gcc -Wall $(CERRORS) $(SRC3) $(LIBS) -o $(PART3)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) \#*
	$(RM) .*.swp
	$(RM) *.core

fclean: clean
	$(RM) $(PART1)
	$(RM) $(PART2)
	$(RM) $(PART3)

re: fclean all
