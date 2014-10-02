#ifndef _HEADER_H_
#define _HEADER_H_

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

#endif