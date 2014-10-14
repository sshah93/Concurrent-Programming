#ifndef _MONITOR_
#define _MONITOR_

#include "cart.h"
#include "q.h"
#include "pthread.h"
#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

pthread_mutex_t lock;

/* Condition Variables */
pthread_cond_t north;
pthread_cond_t west;
pthread_cond_t south;
pthread_cond_t east;


unsigned int direction;
char directions[4];

extern void monitor_init();
extern void monitor_arrive(struct cart_t* cart);
extern void monitor_cross(struct cart_t* cart);
extern void monitor_leave(struct cart_t* cart);
extern void monitor_shutdown();

#endif