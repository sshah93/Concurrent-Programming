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

#ifndef _MONITOR_
#define _MONITOR_

/* all the necessary header files */
#include "cart.h"
#include "q.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* struct to limit the amount of global variables to just 1 */
typedef struct s_env
{
	pthread_mutex_t lock;

	/* Condition Variables, one for each of the four directions */
	pthread_cond_t north;
	pthread_cond_t west;
	pthread_cond_t south;
	pthread_cond_t east;

	char directions[4];				/* Directions array for right-hand rule */
	unsigned int direction;			/* Index into directions */
}	t_env;

/* the only global variable */
t_env gl_env;

/* function to initialize the monitor */
extern void monitor_init();

/* function to control the actions of a cart before it enters the intersection */
extern void monitor_arrive(struct cart_t* cart);

/* function to control the actions of a cart as it crosses the intersection */
extern void monitor_cross(struct cart_t* cart);

/* function to control the actions of a cart after it passes through the intersection */
extern void monitor_leave(struct cart_t* cart);

/* function to free the monitor resources */
extern void monitor_shutdown();

#endif