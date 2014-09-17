/*	Suketu Shah
*	CS-511-A
*	Assignment-1
*	Practice with UNIX Process Mechanisms
*/	

#include "header.h"

typedef enum 
{ 
	false, 
	true 
}	bool;

/* Modified sieve */
bool isPrime(int number)
{
	int i;
	
	if(number == 2) 
	{
		return true;
	}
	
	else if(number < 2 || number % 2 == 0)
	{
		return false;
	}
  
	for(i = 3; (i*i) <= number; i += 2)
	{
		if(number % i == 0)
		{
			return false;
		}
	}

	return true;
}

int main(int argc, char **argv)
{
	int top, bottom, exitcode, i, j, size, num;
	pid_t pids[100];
	int fd[2];
	int readbuffer[1];

	num = argc - 1;
	bottom = 2;
	top = 0; 

	if(argc < 2)
	{
		printf("Usage: ./primes <increasing positive integers>\n");
		return 0;
	}

	top = strtol(argv[1], NULL, 10); /* We can assume good input on this */

	pipe(fd);
  
	for(i = 0; i < num; ++i)
	{
		if((pids[i] = fork()) < 0)
		{
			printf("ERROR: Could not fork child process!\n");
			return 1;
		}
    
		else if(pids[i] == 0)
		{
			printf("child %d: bottom=%d, top=%d\n",getpid(),bottom,top);
			close(fd[0]);

			for(j = bottom; j < top; j++)
			{
				if(isPrime(j))
					write(fd[1], &j, sizeof(j));
			}

			exitcode = -1*getpid();
			write(fd[1], &exitcode, sizeof(exitcode));
			exit(0);
		}
    
		bottom = top + 1;
		
		if(argv[i+2] != NULL) 
		{
			top = strtol(argv[i+2], NULL, 10);
		}
    }
  
	close(fd[1]);

	while(num > 0)
	{
		if((size = read(fd[0], readbuffer, sizeof(readbuffer))) <= 0)
		{
			printf("ERROR: Failed to read from pipe!\n");
			return 1;
		}

		/* decrement num iff we received a negative number */ 
		if(readbuffer[0] < 0)
		{
			printf("child %d exited cleanly.\n",(-1*readbuffer[0]));
			num--;
		}

		if(readbuffer[0] == 0)
		{
			printf("ERROR: Child sent a bad exit code.\n");
			return 1;
		}

		else if(readbuffer[0] > 0)
			printf("%d is prime\n",readbuffer[0]);
	}
	return 0;
}