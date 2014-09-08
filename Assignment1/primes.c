#include "header.h"

#define PIDS_SIZE 200
#define BUFFER_SIZE 1

typedef enum 
{
	false, 
	true
} 	bool; 

bool IsPrime(int num) 
{
    int i;

    for (i=2; i<num; i++)
	{
        if (num % i == 0 && i != num) 
			return false;
    }

    return true;
}

int main(int argc, char** argv)
{
	if(argc <= 1)
	{
		printf("Usage: ./primes <increasing positive integers>\n");
		return 0;
	}
	
	int bottom;
	int top;
	bool flag;
	int countOfArg;
	
	bottom = 2;
	top = argv[1];
	flag = false;
	countOfArg = 1;
	
	if(argc > 2)
	{
		flag = true;
	}
	
	int fd[2];
	int pid;
	int buffer[BUFFER_SIZE];
	pid_t storeAllPid[PIDS_SIZE];
	
	pipe(fd);
	
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
		printf("child %i: bottom=%i, top=%i\n", getpid(), floor, ceiling);
		close(fd[0]); 
	}
	
	/* children */
	else
	{
		printf("child %i: bottom=%i, top=%i\n", getpid(), floor, ceiling);
		close(fd[0]);
	}
}