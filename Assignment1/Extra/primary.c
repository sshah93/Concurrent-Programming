#ifndef NULL
#define NULL (void *)0
#endif
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <errno.h>

#define BUFSIZE 1000

int make_fifo() 
{
	if(mkfifo("./CS511my_probfifo", 0600) < 0) 
		return 0;
	return 1;
}

int read_fifo()
{
	int fd;
	char buf[BUFSIZE];

	if((fd = open("./CS511my_probfifo", O_RDONLY)) < 0) 
	{
		perror("could not read fifo");
		return 0;
	}

	read(fd, buf, BUFSIZE);
	printf("%s\n", buf);
	close(fd);
	return 1;
}

int write_fifo(char * str) 
{
	int fd;
	char buf[BUFSIZE];
	int count = 0;

	if((fd = open("./CS511my_probfifo", O_WRONLY)) < 0) 
	{
		perror("could not write to fifo");
		return 0;
	}

	for(; count < BUFSIZE; count++)
		buf[count] = *str++;

	write(fd, buf, ++count);
	close(fd);
	return 1;
}

int sieve(int floor, int cieling) 
{
	int count= cieling - 1;
	int *array = calloc( cieling + 1, sizeof(int));
	int i, prime, multiple;

	char str[300];
	char answer[BUFSIZE];

	str[0] = '\0';
	answer[0] = '\0';

	/* mark each int as potentially prime */
	for (i=2; i <= cieling; ++i)
		array[i] = 1;

	/* for each starting prime, mark its every multiple as non-prime */
	for (prime = 2; prime <= cieling; ++prime)
	{
		if (array[prime])
		{
			for (multiple = 2*prime; multiple <= cieling; multiple += prime)
			{
				if (array[multiple]) 
				{
					array[multiple] = 0;
					--count;
				}
			}
		}
	}

	/* Now that we have marked all multiples of primes as non-prime, print   */
	/* the remaining numbers that fell through the sieve, and are thus prime */
	
	for (i=floor; i <= cieling; ++i)
	{
		if (array[i]) 
		{
			sprintf(str, "%i is prime\n", i);
			strcat(answer, str);
		}

	}

	write_fifo(answer);
	return 0;
}

int main(int argc, char **argv) 
{
	pid_t pid;
	int i = 1;
	int top, floor;
	make_fifo();

	if (argc < 2)
	{
		fprintf(stderr, "usage: %s 44 100...\n", argv[0]);
		exit(1);
	}

	for(; argv[i] != '\0'; i++)
	{
		pid = fork();

		if (pid < 0)
		{
			exit(1);
		} 
		
		else if (pid == 0)
		{
			if(i == 1)
			{
				floor = 2;
			}
			else floor = atoi(argv[i-1]) + 1;
			top = atoi(argv[i]);

			printf("child %i: bottom=%i, top=%i\n", getpid(), floor, top);
			sieve(floor, top);
			exit(0x47);
		} 
		
		else
		{
			int status = 0;
			read_fifo();
			wait(&status);
			
			if(status == 18176) 
				printf("child %i exited cleanly\n", pid);
			
			else
				printf("unknown exit %i (0x%x)\n", status, status);
		}
	}
	return 0;
}