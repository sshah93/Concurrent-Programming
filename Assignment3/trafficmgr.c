/*
 * CS-511-A
 * Professor Duchamp
 *
 * 10/19/14
 * Authors: Suketu Shah (sshah75) & Michael Peleshenko (mpeleshe)
 *
 * I pledge my honor that I have abided by the Stevens Honor System.
 *
 * Assignment 3
 */

#include "q.h"
#include "monitor.h"
#include <string.h>

/* 
 * Method for threads processing cart queues
 */
void *process_carts(void *arg)
{
	char *in;
	char direction;
	struct cart_t *cart;

	in = (char *) arg;
	direction = *in;

	printf("thread for direction %c starts\n", direction);
	cart = q_getCart(direction);
	while (cart != NULL) {
		printf("thread for direction %c gets cart %i\n", direction, cart->num);
		monitor_arrive(cart);
		monitor_cross(cart);
		monitor_leave(cart);
		cart = q_getCart(direction);
	}
	printf("thread for direction %c exits\n", direction);

	pthread_exit(NULL);
	return NULL;
}

int main(int argc, char *argv[])
{
	unsigned int i;
	unsigned int length;		/* length of argument string */
	char dirs[4];				/* direction for thread initialization */

	pthread_t cart_threads[4];	/* Threads for processing each direction's carts */

	if (argc != 2)
	{
		fprintf(stderr, "usage: %s <arbitrary-length string of [n, w, s, e]>\n", argv[0]);
    	exit(1);
	}

	/* Initialize cart queues for North, West, South, East */
	q_init();

	/* Fill cart queues based on argument string */
	length = strlen(argv[1]);
	for (i = 0; i < length; i++)
	{
		if (argv[1][i] == Q_NORTH
			|| argv[1][i] == Q_WEST
			|| argv[1][i] == Q_EAST
			|| argv[1][i] == Q_SOUTH)
		{
			q_putCart(argv[1][i]);
		}
		else
		{
			fprintf(stderr, "Input string contains character other than [n, w, s, e]\n");
			exit(1);
		}
	}

	/* Initialize the intersection monitor */
	monitor_init();

	/* Initialize threads to process cart queues */
	dirs[0] = Q_NORTH;
	dirs[1] = Q_WEST;
	dirs[2] = Q_SOUTH;
	dirs[3] = Q_EAST;

	for (i = 0; i < 4; i++)
	{
		if(pthread_create(&cart_threads[i], NULL, process_carts, (void *) &dirs[i]) != 0)
		{
			fprintf(stderr, "Failed to create fill thread\n");
			exit(1);
		}
	}

	/* Clean up */

	return 0;
}