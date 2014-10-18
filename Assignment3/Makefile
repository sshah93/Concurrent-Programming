# CS-511-A
# Professor Duchamp
#
# 10/19/14
# Author: Suketu Shah (sshah75) & Michael Peleshenko (mpeleshe)
#
# I pledge my honor that I have abided by the Stevens Honor System.
#
# Assignment 3 - Makefile

NAME = trafficmgr
SRC = q.c \
	monitor.c \
	trafficmgr.c
OBJ = $(SRC:.c=.o)
CC = gcc
CFLAGS += -Wall -pedantic-errors
LIBS = -lpthread

RM = rm -f

all: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) $(LIBS) -o $(NAME)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) \#*
	$(RM) .*.swp
	$(RM) *.core

fclean: clean
	$(RM) $(NAME)

re: fclean all