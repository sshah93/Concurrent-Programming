/*	Suketu Shah
*	CS-511-A
*	Assignment 2 - Part 2
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>

typedef struct	s_args
{
	sem_t* 	semaphore;
	char* 	buffer;
	char* 	output_file;
}		t_args;

t_args threadargs;

void* drainfunc(void* args)
{
	FILE* outfile;
	int nbytes, index, sem_wait_ret, sem_post_ret;

	outfile = NULL;
	index = 0;
	nbytes = 0;
	sem_wait_ret = 0;
	sem_post_ret = 0;

	outfile = fopen(threadargs.output_file, "w");

	if(outfile == NULL)
	{
		printf("Error trying to open the output file for writing!\n");
		exit(1);
	}

	while(1)
	{
		sem_wait_ret = sem_wait(threadargs.semaphore);

		if(sem_wait_ret < 0)
		{
			printf("Drain thread failed on sem_wait!\n");
			pthread_exit(0);
		}

		if(threadargs.buffer[index] == '\0')
		{
			printf("Drain thread: no new string in buffer!\n");
		}

		else if(strcmp(&threadargs.buffer[index], "QUIT") == 0)
		{
			printf("Drain thread: read [QUIT] from buffer\n");
			break;
		}

		else
		{
			nbytes = fputs(&threadargs.buffer[index], outfile);

			if(!nbytes)
			{
				printf("Drain thread: wrote nothing on file!\n");
			}

			else
			{	
				printf("Drain thread: read [%s] from buffer\n", &threadargs.buffer[index]);
				index += strlen(&threadargs.buffer[index]);
				index++;
			}
		}

		sem_post_ret = sem_post(threadargs.semaphore);

		if(sem_post_ret < 0)
		{
			printf("Drain thread failed on sem_post!\n");
			pthread_exit(0);
		}
	}

	pthread_exit(0);
}

int main(int argc, char**argv)
{
	FILE* infile;
	char* input_file;
	int index, i, sleeptime, buffsize, pthread_create_ret, sem_init_ret, sem_wait_ret, sem_post_ret, fclose_ret, sem_destroy_ret, pthread_join_ret;
	char* stripped;
	pthread_t drain;
	
	buffsize = 65535;
	index = 0;
	i = 0;
	sleeptime = 0;

	if(argc != 4)
	{
		printf("Usage: ./transfer1 <input file> <output file> <sleep time in ms> \n");
		exit(1);
	}

	input_file = argv[1];
	threadargs.output_file = argv[2];
	sleeptime = atoi(argv[3]);

	infile = fopen(input_file, "r");

	if(infile == NULL)
	{
		printf("Couldn't open the input file to read!\n");
		exit(1);
	}

	threadargs.buffer = calloc(buffsize, sizeof(char));
	threadargs.semaphore = (sem_t*) malloc(sizeof(sem_t));

	sem_init_ret = sem_init(threadargs.semaphore, 0, 1);

	if(sem_init_ret < 0)
	{
		printf("Failed to initalize the semaphore!\n");
		exit(1);
	}

	pthread_create_ret = pthread_create(&drain, NULL, drainfunc, &threadargs);
	
	if(pthread_create_ret != 0)
	{
		printf("Failed to create drain thread!\n");
		exit(1);
	}
	
	while(1)
	{
		sem_wait_ret = sem_wait(threadargs.semaphore);

		if(sem_wait_ret < 0)
		{
			printf("Fill thread failed on sem_wait!\n");
			exit(1);
		}

		if((fgets(&threadargs.buffer[index], buffsize, infile)) == NULL)
		{
			strcpy(&threadargs.buffer[index], "QUIT");
			printf("Fill thread: wrote %s into buffer\n", &threadargs.buffer[index]);

			sem_post_ret = sem_post(threadargs.semaphore);

			if(sem_post_ret < 0)
			{
				printf("Fill thread failed on sem_post!\n");
				exit(1);
			}

			break;
		}

		stripped = &threadargs.buffer[index];

		for(i = 0; stripped[i] != '\0'; i++)
		{
			if(stripped[i] == '\n')
			{
				stripped[++i] = '\0';
			}
		}

		strcpy(&threadargs.buffer[index], stripped);

		printf("Fill thread: wrote [%s] into buffer\n", &threadargs.buffer[index]);

		index += strlen(&threadargs.buffer[index]) + 1;

		sem_post_ret = sem_post(threadargs.semaphore);

		if(sem_post_ret < 0)
		{
			printf("Fill thread failed on sem_post!\n");
			exit(1);
		}
		
		if(sleeptime != 0)
		{
			usleep(sleeptime);
		}
	}

	fclose_ret = fclose(infile);

	if(fclose_ret != 0)
	{
		printf("Failed to close the input file!\n");
		exit(1);
	}

	pthread_join_ret = pthread_join(drain, NULL);

	if(pthread_join_ret != 0)
	{
		printf("Failed to join the threads!\n");
		exit(1);
	}

	sem_destroy_ret = sem_destroy(threadargs.semaphore);

	if(sem_destroy_ret < 0)
	{
		printf("Failed to destroy the semaphore!\n");
		exit(1);
	}
	
	return 0;
}
