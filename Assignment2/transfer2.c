/*	
*	Suketu Shah
*	CS-511-A
*	Assignment 2 - Part 3
*/

/* Necessary header file includes */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>

/* struct that contains the semaphores, pointer to the output file */
/* stores global variables so we only have 1 global variable for good programming practice */
/* for part 2 of the assignment (transfer1.c) only semaphore1 is used */
/* for part 3 of the assignment (transfer2.c) both semaphores (semaphore1 & semaphore2) are being used */
typedef struct	s_args
{
	/* declaring the semphores */
	sem_t* 	semaphore1;
	sem_t* 	semaphore2;

	/* declaring the buffer needed for local storage while copying line from input to output file */
	char* 	buffer;

	/* pointer to the output file */
	char* 	output_file;
}			t_args;

/* declaring a new object of type t_args */
t_args threadargs;

/* callback function for pthread_create function call */
/* function's return value is void* since pthread_create needs a void* function */
void* callback_function(void* args)
{
	/* declaring the local variables */
	FILE* outfile;
	int nbytes, index, sem_wait_ret, sem_post_ret;

	/* instantiating the variables */
	outfile = NULL;
	index = 0;
	nbytes = 0;
	sem_wait_ret = 0;
	sem_post_ret = 0;

	/* open the output file for writing */
	outfile = fopen(threadargs.output_file, "w");

	/* fopen function return value check */
	if(outfile == NULL)
	{
		printf("Error trying to open the output file for writing!\n");
		exit(1);
	}

	/* main logic of the function resides here */
	/* while loop to keep writing in the output file */
	while(1)
	{
		/* sem_wait function call on the second semaphore to lock the semaphore */
		sem_wait_ret = sem_wait(threadargs.semaphore2);

		/* sem_wait return value check */
		if(sem_wait_ret < 0)
		{
			printf("Drain thread failed on sem_wait!\n");
			pthread_exit(0);
		}

		/* if the last thing we read was \0, then it must have been from the previous line */
		if(threadargs.buffer[index] == '\0')
		{
			printf("Drain thread: no new string in buffer!\n");
		}

		/* if we just read QUIT then we know we are done on the drain thread side */
		else if(strcmp(&threadargs.buffer[index], "QUIT") == 0)
		{
			printf("Drain thread: read [QUIT] from buffer\n");
			break;
		}

		/* or else we still have to write some more line(s) in the output file */ 
		else
		{
			/* write the line stored in the buffer using the function fputs */
			nbytes = fputs(&threadargs.buffer[index], outfile);

			/* fputs return value check */
			if(!nbytes)
			{
				printf("Drain thread: wrote nothing on file!\n");
			}

			/* the print statement required as part of the assignment description to say what we just wrote in the putput file */
			else
			{	
				printf("Drain thread: read [%s] from buffer\n", &threadargs.buffer[index]);
				index += strlen(&threadargs.buffer[index]);
				index++;
			}
		}

		/* unlock the other semaphore */
		sem_post_ret = sem_post(threadargs.semaphore1);

		/* sem_post return value check */
		if(sem_post_ret < 0)
		{
			printf("Drain thread failed on sem_post!\n");
			pthread_exit(0);
		}
	}

	/*  function call to terminate the drain thread */
	pthread_exit(0);
}

int main(int argc, char**argv)
{
	/* local variables */
	FILE* infile;
	char* input_file;
	int index, i, sleeptime, buffsize, pthread_create_ret, sem_init_ret, sem_wait_ret, sem_post_ret, fclose_ret, sem_destroy_ret, pthread_join_ret;
	char* stripped;

	/* the main thread of the program */
	pthread_t drain;
	
	/* arguments check */
	if(argc != 4)
	{
		printf("Usage: ./transfer1 <input file> <output file> <sleep time in ms> \n");
		exit(1);
	}

	/* initializing the variables */
	buffsize = 65535;
	index = 0;
	i = 0;
	sleeptime = 0;
	input_file = argv[1];
	threadargs.output_file = argv[2];
	sleeptime = atoi(argv[3]);

	/* open the input file for reading */
	infile = fopen(input_file, "r");

	/* fopen function return value check */
	if(infile == NULL)
	{
		printf("Couldn't open the input file to read!\n");
		exit(1);
	}

	/* allocating memory for the buffer */
	threadargs.buffer = calloc(buffsize, sizeof(char));

	/* allocate the 2 semaphores */
	threadargs.semaphore1 = (sem_t*) malloc(sizeof(sem_t));
	threadargs.semaphore2 = (sem_t*) malloc(sizeof(sem_t));

	/* initializing the semaphore */
	sem_init_ret = sem_init(threadargs.semaphore1, 0, 1);

	/* sem_init function return value check */
	if(sem_init_ret < 0)
	{
		printf("Failed to initialize the first semaphore!\n");
		exit(1);
	}

	/* initializing the semaphore */
	sem_init_ret = sem_init(threadargs.semaphore2, 0, 0);

	/* sem_init function return value check */
	if(sem_init_ret < 0)
	{
		printf("Failed to initialize the second semaphore!\n");
		exit(1);
	}

	/* creating the drain thread */
	pthread_create_ret = pthread_create(&drain, NULL, callback_function, &threadargs);
	
	/* pthread_create function return value check */
	if(pthread_create_ret != 0)
	{
		printf("Faild to create drain thread!\n");
		exit(1);
	}
	
	/* starting the process to write from input file to the buffer */
	while(1)
	{
		/* lock the first semaphore */
		sem_wait_ret = sem_wait(threadargs.semaphore1);

		/* sem_wait function return value check */
		if(sem_wait_ret < 0)
		{
			printf("Fill thread failed on sem_wait!\n");
			exit(1);
		}

		/* if we couldn't read anymore from the input file */
		if((fgets(&threadargs.buffer[index], buffsize, infile)) == NULL)
		{
			/* write QUIT in the buffer to signal the end of input file */
			strcpy(&threadargs.buffer[index], "QUIT");

			/* print what just got written in the thre */
			printf("Fill thread: wrote %s into buffer\n", &threadargs.buffer[index]);

			/* unlock the second semaphore */
			sem_post_ret = sem_post(threadargs.semaphore2);

			/* sem_post function return value check */
			if(sem_post_ret < 0)
			{
				printf("Fill thread failed on sem_post!\n");
				exit(1);
			}

			break;
		}

		/* bring the stripped pointer to the beginning of the line */
		stripped = &threadargs.buffer[index];

		/* for loop to traverse through the line */
		for(i = 0; stripped[i] != '\0'; i++)
		{
			/* if we see new line that means it's the end of the line, pass \0 to terminate the char* */
			if(stripped[i] == '\n')
			{
				stripped[++i] = '\0';
			}
		}

		/* copy from the stripped pointer all the way to \0 to write that into the buffer */
		strcpy(&threadargs.buffer[index], stripped);

		/* print the statement out as required by the assignment spec */
		printf("Fill thread: wrote [%s] into buffer\n", &threadargs.buffer[index]);

		index += strlen(&threadargs.buffer[index]) + 1;

		/* unlock the second semaphore */
		sem_post_ret = sem_post(threadargs.semaphore2);

		/* sem_post function return value check */
		if(sem_post_ret < 0)
		{
			printf("Fill thread failed on sem_post!\n");
			exit(1);
		}

		/* call the sleeptime in ms as requested in the command line argument */
		usleep(sleeptime);
	}

	/* close the input file */
	fclose_ret = fclose(infile);

	/* fclose function return value check */
	if(fclose_ret != 0)
	{
		printf("Failed to close the input file!\n");
		exit(1);
	}

	/* joining the threads */
	pthread_join_ret = pthread_join(drain, NULL);

	/* pthread_join function return value check */
	if(pthread_join_ret != 0)
	{
		printf("Failed to join the threads!\n");
		exit(1);
	}

	/* destroy the first semaphore */
	sem_destroy_ret = sem_destroy(threadargs.semaphore1);

	/* sem_destroy function return value check */
	if(sem_destroy_ret < 0)
	{
		printf("Failed to destroy the first semaphore!\n");
		exit(1);
	}

	/* destroy the second semaphore */
	sem_destroy_ret = sem_destroy(threadargs.semaphore2);

	/* sem_destroy function return value check */
	if(sem_destroy_ret < 0)
	{
		printf("Failed to destroy the second semaphore!\n");
		exit(1);
	}
	
	return 0;
}