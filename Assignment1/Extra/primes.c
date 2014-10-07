#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

#define PIDS_SIZE 200
#define BUFFER_SIZE 1

int IsPrime(int num) {
    int i;

    for (i=2; i<num; i++)
	{
        if (num % i == 0 && i != num) 
			return 0;
    }
    return 1;
}

int main(int argc, char **argv)
{
	int fd[2]; /* file descriptors */ 
	int floor; /* lower limit */
	int ceiling; /* upper limit */
	int temp; 
	int i;
	int buffer[BUFFER_SIZE]; /* The way professor Gabarro showed us in CS392 */
	pid_t storeAllPid[PIDS_SIZE]; /* array to store all the pids for the parent*/
	
	if(argc < 2) /* We know there's not enough arguments provided to run the program */
	{
		printf("Usage: ./primes <list of increasing positive integers>\n");
		return 0;
	}
	
	pipe(fd); /* Open a pipe */
	floor = 2;
	ceiling = strtol(argv[1], NULL, 10);
	
	/* 1st for loop that will create all the children for us */
	for(i = 0; i < argc - 1; i++)
	{
		/*storeAllPid[i] = fork();
		temp = storeAllPid[i];*/
		
		if(temp < 0) 
		{
			printf("Fork unsuccessful");
			return 1;
		}
		
		else if(temp == 0)
		{			  
			printf("child %i: bottom=%i, top=%i\n", getpid(), floor, ceiling); /* will print out the info for all children */
			close(fd[0]); 
			
			/* Tell the parent all the primes that exist within the range of the floor and ceiling */
			for (i = floor; i < ceiling; i++) /* we will go through every number that lies in the range between floor and ceiling */
			{
				if(IsPrime(i)) /* calls the function IsPrime that will */
					write(fd[1], &i, sizeof(i)); /* write the number if and only if its prime */
			}
			exit(0x47);
		}
		
		floor = ceiling + 1;
		if(argv[i+2] != NULL) /*if we have another number to process*/
		{
		  ceiling = strtol(argv[i+2], NULL, 10);
		}
	}
	
	close(fd[1]);
	
	/* 2nd for loop that will tell us what happened with all the children */
	for(i = 1; i < argc - 1; i++)
	{
		temp = read(fd[0], buffer, BUFFER_SIZE);
		
		if(temp < 1)
			printf("Something went wrong\n"); return 1;

		if(buffer[0] < 0)
			printf("Child %d exited cleanly.\n",(-1*buffer[0]));
			
		if(buffer[0] == 0)
		{
			printf("ERROR: Child sent a bad exit code.\n");
			return 1;
		}
    
		else if(buffer[0] > 0)
			printf("%d is prime\n",buffer[0]);
	}
	return 0;	
}