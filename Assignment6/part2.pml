/* n is global */
byte n;

/* N is the limit */
proctype increment1toN(byte N) {
	byte i = 1;
	byte temp;
	
	do
	:: i > N -> break
	:: else  ->
				temp = n;
				n = temp + 1;
				printf("process %d, i=%d: n changed from %d to %d\n", _pid, i, temp, n);
				i = i + 1
	od
}

init {
	n = 0;
	atomic {
		run  increment1toN(5);
		run  increment1toN(5)
	}
	(_nr_pr == 1) -> printf("at end of simulation n = %d\n", n);
	assert(n!=2);
	printf("simulation passed assertion!\n");
}