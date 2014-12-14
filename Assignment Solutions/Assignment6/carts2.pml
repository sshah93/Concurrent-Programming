/*
 *  Promela does not support variable length arrays
 *  such as cartArray[N].  It also does not support
 *  array arguments such as thread(cartArray).  So
 *  we put all cart numbers into global array "carts"
 *  then pass to each thread an index into that array.
 *  Each thread uses up to MAX_DIRECTION_CARTS carts
 *  from the global array.  Since different threads
 *  may have different numbers of carts, we have to
 *  declare "finished" big enough to accommodate the
 *  largest number of carts that could be used by any
 *  thread.
 */

/* global constants & variables */
#define FREE 0
#define MAX_DIRECTION_CARTS 3     /* largest possible number of carts from any direction */
#define MAX_TOTAL_CARTS 12        /* largest possible number of carts, total */
byte launchPad = FREE;
byte intersection = FREE;
byte carts[MAX_TOTAL_CARTS];


proctype thread(byte startIndex, byte N) {
    byte i;
    byte cart;
    byte finished[MAX_DIRECTION_CARTS];      /* carts that have passed thru intersection */
 
/* move carts according to algorithm */
    i = 0;
    do
    :: i >= N -> break
    :: else ->
    /* get cart */
        cart = carts[startIndex+i];
	printf("thread %d: cart %d, i=%d\n", _pid, cart, i);
    /* arrive */
        atomic {
            launchPad == FREE;          /* wait for launchPad */
            launchPad = cart            /* ... then move in */
        };
    /* cross */
        atomic {
            intersection == FREE;       /* wait for intersection */
            intersection = launchPad;   /* ... then move in */
            launchPad = FREE;
        };
    /* leave */
        finished[i] = intersection;
        intersection = FREE;            /* statement is atomic; don't need "atomic {}" */
    /* next cart */
        i = i + 1
    od;
    
/* verify that all this direction's carts have passed thru intersection in order */
    i = 0;
    do
    :: i >= N -> break
    :: else ->
       assert(carts[startIndex+i] == finished[i]);
       i = i + 1
    od
}


init {
/*
 * Example cart string used for simulation ...
 *
 * carts:  nnewsewses
 * number: 1234567890
 *                  1
 */

/* north: 2 carts in slots 0-1 */
    carts[0] = 1;
    carts[1] = 2;
/* south: 3 carts in slots 2-4 */
    carts[2] = 5;
    carts[3] = 8;
    carts[4] = 10;
/* east: 3 carts in slots 5-7 */
    carts[5] = 3;
    carts[6] = 6;
    carts[7] = 9;
/* west: 2 carts in slots 8-9 */
    carts[8] = 4;
    carts[9] = 7;

/* run only STARTS process; at end of "atomic" all 4 have been started but none has yet executed */    
    atomic {
        run thread(0, 2);   /* north */
        run thread(2, 3);   /* south */
        run thread(5, 3);   /* east */
        run thread(8, 2);   /* west */
    }
}
