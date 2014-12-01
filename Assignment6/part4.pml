#define FREE 0
#define MAX_DIRECTION_CARTS 3
#define MAX_TOTAL_CARTS 12
byte launchPad = FREE;
byte intersection = FREE;
byte carts[MAX_TOTAL_CARTS];
 
proctype thread(byte startIndex, byte N) {
        byte i;
        byte cart;
        byte finished[MAX_DIRECTION_CARTS];
        byte cartArray[MAX_DIRECTION_CARTS];
 
        i = 0;
        do
        :: i >= N -> break
        :: else ->
                cartArray[i] = carts[startIndex+i];
                i = i+1;
        od;
 
        i = 0;
        do
        :: i >= N -> break
        :: else ->
                cart = cartArray[i];
                printf("thread %d: cart %d, i=%d\n", _pid, cart, i);
                /* arrive */
                atomic {
                        launchPad == FREE;
                        launchPad = cart;
                };
                /* cross */
                atomic {
                        intersection == FREE;
                        intersection = launchPad;
                        launchPad = FREE;
                };
                /* leave */
                finished[i] = intersection;
                intersection = FREE;
               
                i = i + 1;
        od;
 
/* verify that all this direction's carts have passed thru intersection in */
        i = 0;
        do
        :: i >= N -> break
        :: else ->
                assert(cartArray[i] == finished[i]);
                i = i + 1;
        od
}
 
init {
/*
* carts: nnewsewses
* number: 1234567890
*/
/* north: 2 carts in slots 0-1 */
        carts[0] = 1;
        carts[1] = 2;
/* south: 3 carts in slots 2-4 */
        carts[2] = 5;
        carts[3] = 8;
        carts[4] = 10;
/* east: 3carts in slots 5-7 */
        carts[5] = 3;
        carts[6] = 6;
        carts[7] = 9;
/* west: 2 carts in slots 8-9 */
        carts[8] = 4;
        carts[9] = 7;
 
        atomic {
                run thread(0, 2);
                run thread(2, 3);
                run thread(5, 3);
                run thread(8, 2);
        }
}