/* CS-511-A
 * Professor Duchamp
 *
 * 10/19/14
 * Authors: Suketu Shah (sshah75) & Michael Peleshenko (mpeleshe)
 *
 * I pledge my honor that I have abided by the Stevens Honor System.
 *
 * Assignment 3
 */
#include "monitor.h"

void monitor_init()
{
	unsigned int i;

	if(pthread_mutex_init(&lock, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the mutex!\n");
		exit(1);
	}

	if(pthread_cond_init(&north, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the north conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&south, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the south conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&east, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the east conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&west, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the west conditional variable!\n");
		exit(1);
	}

	directions[0] = Q_NORTH;
	directions[1] = Q_WEST;
	directions[2] = Q_SOUTH;
	directions[3] = Q_EAST;

	for (i = 0; i < 4; ++i)
	{
		if(q_cartIsWaiting(directions[i]))
		{
			direction = i;
			break;
		}
	}
}

void monitor_arrive(struct cart_t* cart)
{
	if(pthread_mutex_lock(&lock) != 0)
	{
		fprintf(stderr, "Couldn't get the lock!\n");
		exit(1);
	}

	printf("Cart %i arrived at intersection!\n", cart->num);

	while(directions[direction] != cart->dir)
	{
		printf("Cart %i has to wait before it can enter the intersection from %c direction!\n", cart->num, cart->dir);

		switch(cart->dir)
		{
			case Q_NORTH:
				if(pthread_cond_wait(&north, &lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}

			case Q_SOUTH:
				if(pthread_cond_wait(&south, &lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}

			case Q_EAST:
				if(pthread_cond_wait(&east, &lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}

			case Q_WEST:
				if(pthread_cond_wait(&west, &lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}

			default:
				printf("Something is not correct here!\n");
		}

		printf("Cart %i allowed to proceed into intersection!\n", cart->num);
	}
}

void monitor_cross(struct cart_t* cart)
{
	q_cartHasEntered(directions[direction]);

	printf("Cart %i entered the intersection from %c direction!\n", cart->num, cart->dir);
	
	usleep(10000000);

	printf("Cart %i crosses the intersection from %c directon!\n", cart->num, cart->dir);
}

void monitor_leave(struct cart_t* cart)
{
	unsigned int i;

	printf("Cart %i leaves the intersection from %c direction!\n", cart->num, cart->dir);

	/* Check directions to the right for waiting carts */
	for (i = 0; i < 4; i++)
	{
		if(q_cartIsWaiting(directions[(direction + i) % 4]))
		{
			direction = (direction + i) % 4;

			switch(directions[direction])
			{
				case Q_NORTH:
					if(pthread_cond_signal(&north) != 0)
					{
						printf("Thread %c signals thread %c", cart->dir, Q_NORTH);
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
				case Q_WEST:
					if(pthread_cond_signal(&west) != 0)
					{
						printf("Thread %c signals thread %c", cart->dir, Q_WEST);
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
				case Q_SOUTH:
					if(pthread_cond_signal(&south) != 0)
					{
						printf("Thread %c signals thread %c", cart->dir, Q_SOUTH);
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
				case Q_EAST:
					if(pthread_cond_signal(&east) != 0)
					{
						printf("Thread %c signals thread %c", cart->dir, Q_EAST);
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
			}
			break;
		}
	}

	if(pthread_mutex_unlock(&lock) != 0)
	{
		fprintf(stderr, "Failed to unlock the mutex!\n");
		exit(1);
	}
}

void monitor_shutdown()
{
	if(pthread_mutex_destroy(&lock) != 0)
	{
		fprintf(stderr, "Failed to destroy the mutex!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&north) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&west) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&south) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&east) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}
}
