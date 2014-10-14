#ifndef _MONITOR_
#define _MONITOR_

#include "q.h"
#include "pthread.h"
#include "stdio.h"

pthread_mutex_t lock;
pthread_cond_t turn;

int empty;

char direction;

extern void monitor_init();
extern void monitor_arrive(cart_t*);
extern void monitor_cross(cart_t*);
extern void monitor_leave(cart_t*);
extern void monitor_shutdown();

#endif