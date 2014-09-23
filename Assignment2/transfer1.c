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
	sem_t* 	mutex;
	char* 	buffer;
	char* 	output_file;
}		t_args;

t_args threadargs;

void* drain(void* args)
{
	FILE* outfile;
	int nbytes;
	int index;

	index = 0;

	outfile = fopen(threadargs.output_file, "w");

	if(outfile == NULL)
	{
		printf("Error trying to open the output file for writing!\n");
		exit(1);
	}

	while(1)
	{

	}
}

int main(int argc, char**argv)
{
	FILE* infile;
	char* input_file;
	int index, i, sleeptime, buffsize;
	char* stripped;
	
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
		printf("Couldn't open the input file to read!"\n);
		exit(1);
	}
}
