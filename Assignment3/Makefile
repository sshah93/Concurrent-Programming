# CS-511-A
# Professor Duchamp

# 10/19/14
# Author: Suketu Shah (sshah75) & Michael Peleshenko (mpeleshe)

# I pledge my honor that I have abided by the Stevens Honor System.

# Assignment 3
#

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
