NAME = trafficmgr
SRC = trafficmgr.c monitor.c q.c
OBJ = $(SRC:.c=.o)
CFLAGS += -Wall -pedantic-errors
RM = rm -f

all: $(OBJ)
	$(CC) $(CFLAGS) $(SRC) -o $(NAME)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) \#*
	$(RM) .*.swp
	$(RM) *.core

fclean: clean
	$(RM) $(NAME)

re: fclean all
