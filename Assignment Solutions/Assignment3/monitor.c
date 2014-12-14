#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include "cart.h"
#include "q.h"


/*
 *  Monitor for warehouse problem.
 *
 *  There are two wait points:
 *     - "launch pad"
 *     - intersection
 *  At any moment, at most one cart may be on the launch pad;
 *  at any moment, at most one cart may be in the intersection.
 *  One condition variable is associated with the intersection.
 *  Four condition variables -- one for each direction -- are
 *  associated with the launch pad.
 *
 *  The cart on the launch pad is the next one that will enter the
 *  intersection once the intersection becomes available.  The 
 *  4 queues contend for the launch pad.  There is no contention
 *  for the intersection: the cart on the launch pad moves into the
 *  intersection once it is signaled that the intersection is available.
 *
 *  The basic logic of the 3 monitor procedures is:
 *
 *  monitor_arrive:
 *      get monitor lock
 *      while ((launch pad occupied) OR (not my turn))
 *          wait(launch pad CV)
 *      move cart into launch pad
 *      // no signal operation
 *      drop monitor lock
 *
 *  monitor_cross:
 *      get monitor lock
 *      while (intersection occupied)
 *          wait(intersection CV)
 *      move cart from launch pad into intersection
 *      signal next non-empty direction to the right to enter launch pad
 *      drop monitor lock
 *
 *  monitor_leave:
 *      get monitor lock
 *      // no wait operation
 *      mark intersection unoccupied
 *      signal intersection CV
 *      drop monitor lock
 *
 *  There are two tricky points that complicate the program ...
 *
 *  The first tricky point is determining whether a cart is waiting in
 *  a particular direction.  The main loop in trafficmgr:moveCart
 *  removes the first element from the queue and holds it in variable
 *  "cart."  Therefore, the set of carts waiting for a particular
 *  direction is: {cart variable, queue for that direction}.  Testing
 *  whether the queue is empty will NOT correctly determine whether a
 *  cart is waiting for that direction.  For this reason the "q" module
 *  has a "cartIsWaiting" variable for each direction.  The variable is
 *  set to true by q_getCart when a cart is removed the queue.  The variable
 *  is set to false by q_cartHasEntered -- therefore q_cartHasEntered must
 *  be called by monitor_arrive once a cart from that direction is placed
 *  on the launch pad.  Function q_cartIsWaiting() returns the value of
 *  the variable.
 *
 *  Another tricky point is handling the beginning and end of a run.
 *  At the beginning of a run, threads for some directions may be running
 *  even before threads for other directions have started.  There is no
 *  point in, say, north signaling west if the west thread isn't running
 *  yet.  At the end of a run, there may be carts from only one direction.
 *  In this case, q_cartIsWaiting() will return false for ALL directions!
 *  It will return false for the 3 other directions because there are no
 *  carts in those directions.  It will return false for the same direction
 *  because a thread calls q_cartIsWaiting() in monitor_cross; at that point
 *  the direction's "cartIsWaiting" variable has been set to false by the
 *  call to q_cartHasEntered earlier in monitor_arrive.  To address both
 *  these cases I invented a new direction 'x' that means that a cart from
 *  any direction can proceed to the launch pad (provided the launch pad
 *  is unoccupied of course).  If the next direction is 'x' no signal is
 *  sent.  The program starts with it being the turn of 'x' direction.
 *  Also, in signalNextNonemptyQueueToTheRight if the code finds that
 *  q_cartIsWaiting() returns false in all 4 directions then it sets
 *  "nextToProceed" to 'x' in case there are more carts in same direction.
 */


/* these variables implement the monitor */
static pthread_mutex_t monLock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t northAccessLaunchPad = PTHREAD_COND_INITIALIZER;
static pthread_cond_t southAccessLaunchPad = PTHREAD_COND_INITIALIZER;
static pthread_cond_t eastAccessLaunchPad = PTHREAD_COND_INITIALIZER;
static pthread_cond_t westAccessLaunchPad = PTHREAD_COND_INITIALIZER;
static pthread_cond_t accessIntersection = PTHREAD_COND_INITIALIZER;
/* these state variables are protected by the monitor */
static struct cart_t *launchPad = NULL;
static struct cart_t *intersection = NULL;
static char nextToProceed = 'x';       /* x signifies any direction;
				          should be Q_ANY but don't want to change Q interface */


void monitor_init() {
/*
 *  All monitor variables are initialized statically above.
 *  Alternatively, could make calls here to init mutex & CVs.
 */
  return;
}


void monitor_arrive(struct cart_t *cart) {
  int num = cart->num;
  char dir = cart->dir;

  pthread_mutex_lock(&monLock);
#if 1
  fprintf(stderr, "cart %i from direction %c contends for launch pad (nextToProceed is %c)\n",
	  num, dir, nextToProceed);
#endif
  /* wait while (1) launch pad occupied, or (2) it's another direction's turn */
  while ((launchPad != NULL) ||
	 ((nextToProceed != dir) && (nextToProceed != 'x'))) {
    if (dir == Q_NORTH) {
      pthread_cond_wait(&northAccessLaunchPad, &monLock);
    } else if (dir == Q_SOUTH) {
      pthread_cond_wait(&southAccessLaunchPad, &monLock);
    } else if (dir == Q_EAST) {
      pthread_cond_wait(&eastAccessLaunchPad, &monLock);
    } else if (dir == Q_WEST) {
      pthread_cond_wait(&westAccessLaunchPad, &monLock);
    }
  }
  launchPad = cart;
  q_cartHasEntered(dir);    /* this is important; see comment at top of file */
  fprintf(stderr, "cart %i from direction %c arrives at intersection\n", num, dir);
  /* no signal operation */
  pthread_mutex_unlock(&monLock);
  return;
}


static void signalNextNonemptyQueueToTheRight(int signaler, char directionOfSignaler) {

  /* determine next direction to proceed to launch pad */
  fprintf(stderr, "cart from direction ");
  switch (directionOfSignaler) {
  case Q_NORTH:
    if (q_cartIsWaiting(Q_WEST)) {
      fprintf(stderr, "w");
      nextToProceed = Q_WEST;
    } else if (q_cartIsWaiting(Q_SOUTH)) {
      fprintf(stderr, "s");
      nextToProceed = Q_SOUTH;
    } else if (q_cartIsWaiting(Q_EAST)) {
      fprintf(stderr, "e");
      nextToProceed = Q_EAST;
    } else if (q_cartIsWaiting(Q_NORTH)) {
      fprintf(stderr, "n");
      nextToProceed = Q_NORTH;
    } else {
      fprintf(stderr, "ANY");
      nextToProceed = 'x';
    }
    break;
  case Q_SOUTH:
    if (q_cartIsWaiting(Q_EAST)) {
      fprintf(stderr, "e");
      nextToProceed = Q_EAST;
    } else if (q_cartIsWaiting(Q_NORTH)) {
      fprintf(stderr, "n");
      nextToProceed = Q_NORTH;
    } else if (q_cartIsWaiting(Q_WEST)) {
      fprintf(stderr, "w");
      nextToProceed = Q_WEST;
    } else if (q_cartIsWaiting(Q_SOUTH)) {
      fprintf(stderr, "s");
      nextToProceed = Q_SOUTH;
    } else {
      fprintf(stderr, "ANY");
      nextToProceed = 'x';
    }
    break;
  case Q_EAST:
    if (q_cartIsWaiting(Q_NORTH)) {
      fprintf(stderr, "n");
      nextToProceed = Q_NORTH;
    } else if (q_cartIsWaiting(Q_WEST)) {
      fprintf(stderr, "w");
      nextToProceed = Q_WEST;
    } else if (q_cartIsWaiting(Q_SOUTH)) {
      fprintf(stderr, "s");
      nextToProceed = Q_SOUTH;
    } else if (q_cartIsWaiting(Q_EAST)) {
      fprintf(stderr, "e");
      nextToProceed = Q_EAST;
    } else {
      fprintf(stderr, "ANY");
      nextToProceed = 'x';
    }
    break;
  case Q_WEST:
    if (q_cartIsWaiting(Q_SOUTH)) {
      fprintf(stderr, "s");
      nextToProceed = Q_SOUTH;
    } else if (q_cartIsWaiting(Q_EAST)) {
      fprintf(stderr, "e");
      nextToProceed = Q_EAST;
    } else if (q_cartIsWaiting(Q_NORTH)) {
      fprintf(stderr, "n");
      nextToProceed = Q_NORTH;
    } else if (q_cartIsWaiting(Q_WEST)) {
      fprintf(stderr, "w");
      nextToProceed = Q_WEST;
    } else {
      fprintf(stderr, "ANY");
      nextToProceed = 'x';
    }
    break;
  }
  fprintf(stderr, " may proceed to launch pad\n");

  /* if another direction is waiting, give it a signal */
  if (nextToProceed == Q_NORTH) {
    fprintf(stderr, "cart %i from %c signals n to proceed to launch pad\n", signaler, directionOfSignaler);
    pthread_cond_signal(&northAccessLaunchPad);
  } else if (nextToProceed == Q_SOUTH) {
    fprintf(stderr, "cart %i from %c signals s to proceed to launch pad\n", signaler, directionOfSignaler);
    pthread_cond_signal(&southAccessLaunchPad);
  } else if (nextToProceed == Q_EAST) {
    fprintf(stderr, "cart %i from %c signals e to proceed to launch pad\n", signaler, directionOfSignaler);
    pthread_cond_signal(&eastAccessLaunchPad);
  } else if (nextToProceed == Q_WEST) {
    fprintf(stderr, "cart %i from %c signals w to proceed to launch pad\n", signaler, directionOfSignaler);
    pthread_cond_signal(&westAccessLaunchPad);
  }

  return;
}


void monitor_cross(struct cart_t *cart) {
  int num = cart->num;
  char dir = cart->dir;
  int i;

  pthread_mutex_lock(&monLock);
  while (intersection != NULL) {
    fprintf(stderr, "cart %i from direction %c must wait for intersection\n", num, dir);
    pthread_cond_wait(&accessIntersection, &monLock);
  }
  intersection = cart;       /* these 2 lines together ... */
  launchPad = NULL;          /* ... move cart from launch pad into intersection */
  fprintf(stderr, "cart %i from direction %c enters intersection\n", num, dir);
  signalNextNonemptyQueueToTheRight(num, dir);     /* next cart to the right can enter launch pad */
  pthread_mutex_unlock(&monLock);

/* print a time lapse depiction of cart crossing intersection */
#define QUARTER_SECOND 250000
#define TEN_SECONDS 40
  for (i=0; i<TEN_SECONDS; i++) {
    usleep(QUARTER_SECOND);
    if (i == 0) {
      fprintf(stderr, "cart %i from direction %c crosses intersection: ", num, dir);
    } else {
      if (i % 2 == 0)
	fprintf(stderr, "%i", num);
      else
	fprintf(stderr, "%c", dir);
    }
  }
  fprintf(stderr, " done\n");
}


void monitor_leave(struct cart_t *cart) {
  int num = cart->num;
  char dir = cart->dir;

  pthread_mutex_lock(&monLock);
  /* no wait operation */
  intersection = NULL;
  fprintf(stderr, "cart %i from direction %c leaves intersection\n", num, dir);
  pthread_cond_signal(&accessIntersection);    /* OK for cart on launch pad to enter intersection */
  pthread_mutex_unlock(&monLock);
  return;
}


void monitor_shutdown() {
  pthread_mutex_destroy(&monLock);
  pthread_cond_destroy(&northAccessLaunchPad);
  pthread_cond_destroy(&southAccessLaunchPad);
  pthread_cond_destroy(&eastAccessLaunchPad);
  pthread_cond_destroy(&westAccessLaunchPad);
  pthread_cond_destroy(&accessIntersection);
  return;
}
