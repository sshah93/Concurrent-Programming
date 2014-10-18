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

#define THREADS 4 /* Number of threads processing carts */

/* Barrier for threads */
static pthread_barrier_t barrier;

/* 
 *	Method for threads processing cart queues
 *	Pre: function takes a void* as an argument
 *	Post: function returns a void* as the return value
 */
void *process_carts(void *arg)
{
	char *in;
	char direction;
	struct cart_t *cart;

	/* retrieve the char that contains the next direction */
	in = (char *) arg;
	direction = *in;

	printf("thread for direction %c starts\n", direction);
	cart = q_getCart(direction);

	/* critical section of the algorithm */
	while (cart != NULL) {
		printf("thread for direction %c gets cart %i\n", direction, cart->num);
		monitor_arrive(cart);
		monitor_cross(cart);
		monitor_leave(cart);
		cart = q_getCart(direction);
	}

	pthread_barrier_wait(&barrier);

	printf("thread for direction %c exits\n", direction);

	pthread_exit(NULL);
	return NULL;
}

int main(int argc, char *argv[])
{
	unsigned int i;				/* needed for "for" loops */
	unsigned int length;		/* length of argument string */
	char dirs[4];				/* direction for thread initialization */

	pthread_t cart_threads[4];	/* Threads for processing each direction's carts */

	/* check the number of arguments */
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

	/* Initialize barrier for threads to end at the same time */
	pthread_barrier_init(&barrier, NULL, THREADS);

	/* Initialize threads to process cart queues */
	dirs[0] = Q_NORTH;
	dirs[1] = Q_WEST;
	dirs[2] = Q_SOUTH;
	dirs[3] = Q_EAST;

	/* create the four cart threads, one for each direction */
	for (i = 0; i < THREADS; i++)
	{
		if(pthread_create(&cart_threads[i], NULL, process_carts, (void *) &dirs[i]) != 0)
		{
			fprintf(stderr, "Failed to create cart thread %c\n", dirs[i]);
			exit(1);
		}
	}

	/* Join threads */
	for (i = 0; i < THREADS; i++)
	{
		if(pthread_join(cart_threads[i], NULL) != 0)
		{
			fprintf(stderr, "Failed to join thread\n");
			exit(1);
		}
	}

	/* Clean up */
	q_shutdown();
	monitor_shutdown();
	if(pthread_barrier_destroy(&barrier) != 0)
	{
		fprintf(stderr, "Failed to destroy barrier\n");
		exit(1);
	}

	return 0;
}