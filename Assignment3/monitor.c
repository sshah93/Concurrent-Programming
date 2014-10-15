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

	if(pthread_mutex_init(&gl_env.lock, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the mutex!\n");
		exit(1);
	}

	if(pthread_cond_init(&gl_env.north, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the north conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&gl_env.south, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the south conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&gl_env.east, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the east conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_init(&gl_env.west, NULL) != 0)
	{
		fprintf(stderr, "Failed to initialize the west conditional variable!\n");
		exit(1);
	}

	gl_env.directions[0] = Q_NORTH;
	gl_env.directions[1] = Q_WEST;
	gl_env.directions[2] = Q_SOUTH;
	gl_env.directions[3] = Q_EAST;

	for (i = 0; i < 4; ++i)
	{
		if(q_cartIsWaiting(gl_env.directions[i]))
		{
			gl_env.direction = i;
			break;
		}
	}
	/* First direction will be set in monitor_arrive */
	gl_env.direction = -1;

	/* Set interesection to empty state */
	gl_env.empty = 1;
}

void monitor_arrive(struct cart_t* cart)
{
	unsigned int i;

	if(pthread_mutex_lock(&gl_env.lock) != 0)
	{
		fprintf(stderr, "Couldn't get the lock!\n");
		exit(1);
	}

	printf("Cart %i from direction %c arrives at intersection!\n", cart->num, cart->dir);

	/* Set initial direction based on first cart to arrive */
	if (gl_env.direction == -1)
	{
		for (i = 0; i < 4; i++)
		{
			if (cart->dir == gl_env.directions[i])
			{
				gl_env.direction = i;
				break;
			}
		}
	}

	while(gl_env.directions[gl_env.direction] != cart->dir)
	{
		printf("Cart %i from direction %c must wait before entering intersection!\n", cart->num, cart->dir);

		switch(cart->dir)
		{
			case Q_NORTH:
			{
				if(pthread_cond_wait(&gl_env.north, &gl_env.lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}
				break;
			}

			case Q_WEST:
			{
				if(pthread_cond_wait(&gl_env.west, &gl_env.lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}
				break;
			}

			case Q_SOUTH:
			{
				if(pthread_cond_wait(&gl_env.south, &gl_env.lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}
				break;
			}

			case Q_EAST:
			{
				if(pthread_cond_wait(&gl_env.east, &gl_env.lock) != 0)
				{
					fprintf(stderr, "Wait function messed up!\n");
					exit(1);
				}
				break;
			}

			default:
			{
				fprintf(stderr, "Something is not correct here!\n");
				exit(1);
			}
		}

		printf("Cart %i from direction %c allowed to proceed into intersection!\n", cart->num, cart->dir);
	}

	/* Set intersection state to be occupied */
	gl_env.empty = 0;

	/* Drop the lock */
	if(pthread_mutex_unlock(&gl_env.lock) != 0)
	{
		fprintf(stderr, "Failed to unlock the mutex!\n");
		exit(1);
	}
}

void monitor_cross(struct cart_t* cart)
{
	q_cartHasEntered(gl_env.directions[gl_env.direction]);

	printf("Cart %i from direction %c enters intersection!\n", cart->num, cart->dir);
	
	usleep(10000000);

	printf("Cart %i from direction %c crosses intersection!\n", cart->num, cart->dir);
}

void monitor_leave(struct cart_t* cart)
{
	unsigned int i;

	/* Get the lock */
	if(pthread_mutex_lock(&gl_env.lock) != 0)
	{
		fprintf(stderr, "Couldn't get the lock!\n");
		exit(1);
	}

	printf("Cart %i from direction %c leaves intersection!\n", cart->num, cart->dir);

	/* Set intersection state to be empty */
	gl_env.empty = 1;

	/* Check directions to the right for waiting carts */
	for (i = 1; i <= 4; i++)
	{
		if(q_cartIsWaiting(gl_env.directions[(gl_env.direction + i) % 4]))
		{
			gl_env.direction = (gl_env.direction + i) % 4;

			switch(gl_env.directions[gl_env.direction])
			{
				case Q_NORTH:
				{
					printf("Thread %c signals thread %c\n", cart->dir, Q_NORTH);
					if(pthread_cond_signal(&gl_env.north) != 0)
					{
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
					break;
				}
				case Q_WEST:
				{
					printf("Thread %c signals thread %c\n", cart->dir, Q_WEST);
					if(pthread_cond_signal(&gl_env.west) != 0)
					{
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
					break;
				}
				case Q_SOUTH:
				{
					printf("Thread %c signals thread %c\n", cart->dir, Q_SOUTH);
					if(pthread_cond_signal(&gl_env.south) != 0)
					{
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
					break;
				}
				case Q_EAST:
				{
					printf("Thread %c signals thread %c\n", cart->dir, Q_EAST);
					if(pthread_cond_signal(&gl_env.east) != 0)
					{
						fprintf(stderr, "Failed to signal!\n");
						exit(1);
					}
					break;
				}

				default:
				{
					fprintf(stderr, "Something went wrong\n");
					exit(1);
				}
			}

			/* Break when a waiting cart is found */
			break;
		}
	}

	/* Drop the lock */
	if(pthread_mutex_unlock(&gl_env.lock) != 0)
	{
		fprintf(stderr, "Failed to unlock the mutex!\n");
		exit(1);
	}
}

void monitor_shutdown()
{
	if(pthread_mutex_destroy(&gl_env.lock) != 0)
	{
		fprintf(stderr, "Failed to destroy the mutex!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&gl_env.north) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&gl_env.west) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&gl_env.south) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}

	if(pthread_cond_destroy(&gl_env.east) != 0)
	{
		fprintf(stderr, "Failed to destroy the conditional variable!\n");
		exit(1);
	}
}
