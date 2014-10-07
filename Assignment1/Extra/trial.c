#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

#ifndef PIDS_SIZE
#define PIDS_SIZE 200
#endif

int IsPrime(int num) 
{
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
	pid_t storeAllPid[PIDS_SIZE]; /* The way professor Gabarro showed us in CS392. An array to store all the pids for the parent*/
	
	if(argc < 2) /* We know there's not enough arguments provided to run the program */
	{
		printf("Usage: ./primes <list of increasing positive integers>\n");
		return 0;
	}
	
	pipe(fd);
	floor = 2;
	ceiling = atoi(argv[1]);
	
	/* 1st for loop that will create all the children for us */
	for(i = 1; i < argc+1; i++)
	{
		storeAllPid[i] = fork();
		temp = storeAllPid[i];
	
		if(temp < 0) 
		{
			printf("Fork unsuccessful");
			exit(1);
		}
		
		else if(temp == 0)
		{			  
			if(i != 1) /* case when we are not at the first argument anymore*/
				floor = strtol(argv[i-1], NULL, 10) + 1; 
				
			ceiling = strtol(argv[i], NULL, 10);
			
			printf("child %i: floor=%i, ceiling=%i\n", getpid(), floor, ceiling); /* will print out the info for all children */
			close(fd[0]); 
			
			/* Tell the parent all the primes that exist within the range of the floor and ceiling */
			for (i = floor; i < ceiling; i++) /* we will go through every number that lies in the range between floor and ceiling */
			{
				if(IsPrime(i)) /* calls the function IsPrime that will */
					write(fd[1], &i, sizeof(i)); /* write the number if and only if its prime */
			}

			exit(0x47);
		} 
	}
	
	close(fd[1]);
	return 0;	
}