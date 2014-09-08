#include "header.h"

#define PIDS_SIZE 200
#define BUFFER_SIZE 1

typedef enum 
{
	false, 
	true
} 	bool; 

/* Modified sieve */
bool isPrime(int number)
{
	int i;
	if(number < 2) 
		return false;

	if(number == 2) 
		return true;

	if(number % 2 == 0) 
		return false;
  
	for(i = 3; (i*i)<=number; i+=2)
	{
		if(number % i == 0 ) 
		return false;
	}

	return true;
}

int main(int argc, char** argv)
{
	int top, bottom, countOfArg, pid, i;
	int fd[2];
	int buffer[BUFFER_SIZE];
	pid_t storeAllPid[PIDS_SIZE];
	
	if(argc <= 1)
	{
		printf("Usage: ./primes <increasing positive integers>\n");
		return 0;
	}
	
	bottom = 2;
	top = strtol(argv[1], NULL, 10);
	countOfArg = argc - 1;

	pipe(fd);
	
	for(i = 0; i < countOfArg; ++i)
	{
		if(i != 0)
		{
			bottom = top + 1;
			top = strtol(argv[i+1], NULL, 10);
		}
		
		/* creating new children, will have to put this in loop */
		pid = fork();
	
		/* Trouble */
		if(pid < 0)
		{
			printf("Problem in function call fork. Your child didn't spawn!\n");
			return 1;
		}
		
		/* Parent part */
		else if(pid > 0)
		{
			/* printf("child %i: bottom=%i, top=%i\n", getpid(), bottom, top); */
			/* close(fd[0]); */
		}
		
		/* children */
		else
		{
			printf("child %i: bottom=%i, top=%i\n", getpid(), bottom, top);
			close(fd[0]);
		}
	}
}