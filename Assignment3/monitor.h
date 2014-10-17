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

#include "cart.h"
#include "q.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct s_env
{
	pthread_mutex_t lock;

	/* Condition Variables */
	pthread_cond_t north;
	pthread_cond_t west;
	pthread_cond_t south;
	pthread_cond_t east;

	char directions[4];				/* Directions array for right-hand rule */
	unsigned int direction;			/* Index into directions */
}	t_env;

t_env gl_env;

extern int isIntersectionEmpty();
extern void monitor_init();
extern void monitor_arrive(struct cart_t* cart);
extern void monitor_cross(struct cart_t* cart);
extern void monitor_leave(struct cart_t* cart);
extern void monitor_shutdown();

#endif
