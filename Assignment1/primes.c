/*	Suketu Shah
*	CS-511-A
*	Assignment-1
*	Practice with UNIX Process Mechanisms
*/	

/* 	Project Description: The purpose of this assignment is to acquire experience with UNIX mechanisms that are helpful in writing multi-process programs. 
*	Program called "primes" accepts a sequence of increasing positive numbers on the command line. 
*	The integers define range and a child process is created to search for primes in each range. 
*/

/* All the header files required for this assignment */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <limits.h>

/* struct to have a boolean data type in C like C++ */
typedef enum 
{ 
	false, 
	true 
}	bool;

/* Pre: takes an integer to identify if the number is a prime or not */
/* Post: returns true(1) if the number is a prime number or else returns false(0) */
/* Modified sieve */
bool isPrime(int number)
{
	int i;
	
	if(number == 2) 
	{
		return true;
	}
	
	/* case when the input is 1 or any even number other than 2 */
	else if(number < 2 || number % 2 == 0)
	{
		return false;
	}
	
	/* checking for gcd */
	for(i = 3; (i*i) <= number; i += 2)
	{
		if(number % i == 0)
		{
			return false;
		}
	} /* end of for loop */

	return true;
} /* end of isPrime function */


/* start of main */
int main(int argc, char **argv)
{
	/* declaring all the variables */
	long upper_range;
	int top, bottom, exitcode, i, j, size, num;
	pid_t pids[100];
	int fd[2];
	int readbuffer[1];

	/* initialize the variables */
	num = argc - 1;
	bottom = 2;
	top = 0; 

	/* base case check if enough arguments are supplied or not */
	if(argc < 2)
	{
		printf("Usage: ./primes <increasing positive integers>\n");
		return 0;
	}

	upper_range = strtol(argv[1], NULL, 10); 
	
	/* check for the return value from strtol function call to determine if there were any exceptions or not */
	if(upper_range == LONG_MIN || upper_range == LONG_MAX)
	{
		printf("Either underflow or overflow error occurred when converting the number using the function strtol\n");
		return 1;
	}
	
	/* else store it in the int */
	else
	{
		top = upper_range;
	}

	pipe(fd);
  
	/* for loop to create the necessary number of children based on the number of input arguments */
	for(i = 0; i < num; ++i)
	{
		/* fork call to spawn a new child */
		pids[i] = fork();
		
		/* check the value of pids[i] to see if the fork call was successful or not */
		if(pids[i] < 0)
		{
			printf("ERROR: Could not fork child process!\n");
			return 1;
		}
    
		/* the case for children */
		else if(pids[i] == 0)
		{
			/* print the pid of the child and ranges it will be looking over for primes */
			printf("child %d: bottom=%d, top=%d\n",getpid(),bottom,top);
			
			/* close the file descriptor */
			close(fd[0]);

			/* for all the numbers in the range go through each of them one by one and call the isPrime function on each number */
			for(j = bottom; j < top; j++)
			{
				if(isPrime(j))
					write(fd[1], &j, sizeof(j));
			}

			/* prepare the proper exit code for the parent */
			exitcode = -1*getpid();
			
			/* write the exit code so the parent knows if the child exited clenly or not */
			write(fd[1], &exitcode, sizeof(exitcode));
			
			/* exit from the child as its purpose is now over */
			exit(0);
		}
    
		/* change the bottom range for the next child */
		bottom = top + 1;
		
		/* check if there are any more arguments left from stdin to determine if we will be spawning a new child or not */
		if(argv[i+2] != NULL) 
		{
			upper_range = strtol(argv[i+2], NULL, 10);
			
			/* check for the return value from strtol function call to determine if there were any exceptions or not */
			if(upper_range == LONG_MIN || upper_range == LONG_MAX)
			{
				printf("Either underflow or overflow error occurred when converting the number using the function strtol\n");
				return 1;
			}
			
			/* else store it in the int */
			else
			{
				top = upper_range;
			}
		}
    } /* end of for loop */
  
	close(fd[1]);

	/* dealing with the parent stuff related to printing and clean exit now */
	while(num > 0)
	{
		/* try to read from the buffer to make sure we can do in order to read all the primes from there on and if not able to read, complain and exit the program */
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

		/* check for the exit code received from the child to verify the child died cleanly or not */
		if(readbuffer[0] == 0)
		{
			printf("ERROR: Child sent a bad exit code.\n");
			return 1;
		}

		/* print out the prime number from the pipe */
		else if(readbuffer[0] > 0)
		{
			printf("%d is prime\n",readbuffer[0]);
		}
	} /* end of while loop */
	
	/* final return to terminate the program */
	return 0;
} /* end of main */
