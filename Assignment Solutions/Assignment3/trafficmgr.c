#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "q.h"
#include "monitor.h"


static pthread_barrier_t endBarrier;


/*
 *  Each thread repeatedly does this:
 *
 *  Try to get a cart off the head of the indicated queue.
 *  If there is no cart on that queue, return.
 *  If a cart is obtained, then:
 *     1. "arrive" at intersection -- may have to wait until signaled
 *     2. proceed into intersection once intersection is free & it becomes our turn
 *     3. leave intersection & signal closest non-empty queue to the right
 */
void *moveCart(void *args) {
  char direction = *(char *)args;
  struct cart_t *cart;

  fprintf(stderr, "thread for direction %c starts\n", direction);
  cart = q_getCart(direction);
  while (cart != NULL) {
    fprintf(stderr, "thread for direction %c removes cart %i from head of queue\n", direction, cart->num);
    monitor_arrive(cart);
    monitor_cross(cart);
    monitor_leave(cart);
    cart = q_getCart(direction);
  }
#if 1
  (void) pthread_barrier_wait(&endBarrier);
#endif
  fprintf(stderr, "thread for direction %c ends\n", direction);
  return NULL;
}


int main(int argc, char *argv[]) {
  char *directions;
  int i;
  pthread_t tid[4];
  char *arg;
  int rc;

/* get command line */
  if (argc != 2) {
    fprintf(stderr, "usage: %s <string: any length/combination of n,e,w,s>\n", argv[0]);
    exit(1);
  }
  directions = argv[1];

/* init other modules */
  q_init();
  monitor_init();
  pthread_barrier_init(&endBarrier, NULL, 4);

/* 1. place all carts on appropriate queue */
  for (i=0; i<strlen(directions); i++)
    q_putCart(directions[i]);
#if 1
  q_print(Q_NORTH);
  q_print(Q_SOUTH);
  q_print(Q_EAST);
  q_print(Q_WEST);
#endif

/* 2. create 4 threads, one for each direction */
  arg = malloc(sizeof(char));
  *arg = Q_NORTH;
  if ((rc = pthread_create(&tid[0], NULL, moveCart, (void *)arg)) != 0)
    fprintf(stderr, "thread create %c failed (%s)\n", *arg, strerror(rc)), exit(1);
  arg = malloc(sizeof(char));
  *arg = Q_SOUTH;
  if ((rc = pthread_create(&tid[1], NULL, moveCart, (void *)arg)) != 0)
    fprintf(stderr, "thread create %c failed (%s)\n", *arg, strerror(rc)), exit(1);
  arg = malloc(sizeof(char));
  *arg = Q_EAST;
  if ((rc = pthread_create(&tid[2], NULL, moveCart, (void *)arg)) != 0)
    fprintf(stderr, "thread create %c failed (%s)\n", *arg, strerror(rc)), exit(1);
  arg = malloc(sizeof(char));
  *arg = Q_WEST;
  if ((rc = pthread_create(&tid[3], NULL, moveCart, (void *)arg)) != 0)
    fprintf(stderr, "thread create %c failed (%s)\n", *arg, strerror(rc)), exit(1);

/* 3. join threads as they complete */
  if ((rc = pthread_join(tid[0], NULL)) != 0)
    fprintf(stderr, "thread join 0 failed (%s)\n", strerror(rc)), exit(1);
  if ((rc = pthread_join(tid[1], NULL)) != 0)
    fprintf(stderr, "thread join 1 failed (%s)\n", strerror(rc)), exit(1);
  if ((rc = pthread_join(tid[2], NULL)) != 0)
    fprintf(stderr, "thread join 2 failed (%s)\n", strerror(rc)), exit(1);
  if ((rc = pthread_join(tid[3], NULL)) != 0)
    fprintf(stderr, "thread join 3 failed (%s)\n", strerror(rc)), exit(1);

/* release resources */
  q_shutdown();
  monitor_shutdown();
  pthread_barrier_destroy(&endBarrier);

  return 0;
}
