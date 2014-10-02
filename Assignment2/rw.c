/*	
*	Suketu Shah
*	CS-511-A
*	Assignment 2 - Part 1
*/

/* Necessary file includes */
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char**argv)
{
	/* local variables */
	FILE* input_file;
	FILE* output_file;
	char* line;
	size_t line_length;
	ssize_t bytes_read;
	int bytes_wrote;

	/* initialization */
	line_length = 10;
	
	/* creating a char* of length 11 to store the line read from the input file */
	/* getline function call will allocate more if need be */
	line = (char*) malloc(line_length + 1);
	
	/* malloc function return value check */
	if(line == NULL)
	{
		printf("Some error occured while trying to allocate memory using function malloc! \n");
		exit(1);
	}

	/* Number of args check */
	if(argc != 3)
	{
		printf("Usage: ./rw <input file> <output file> \n");
		exit(1);
	}

	/* opening the input file */
	input_file = fopen(argv[1], "r");

	/* fopen function return value check */
	if(input_file == NULL)
	{
		printf("Couldn't open the input file for reading \n");
		exit(1);
	}

	/* opening the output file */
	output_file = fopen(argv[2], "w");

	/* fopen function return value check */
	if(output_file == NULL)
	{
		printf("Output file couldn't be open to write to the file \n");
		exit(1);
	}

	/* the main logic of the program resides here */
	/* using the getline function, I'm reading each line from the input file */
	/* getline function returns -1 when the end of file is reached */
	while((bytes_read = getline(&line, &line_length, input_file)) != -1)
	{
		/* after reading each line, fwrite function is used to write the line to the output file */
		bytes_wrote = fwrite(line, sizeof(char), bytes_read, output_file);

		/* fwrite function return value check */
		if(bytes_wrote < 1)
		{
			printf("Problem writing to the output file!\n");
			exit(1);
		}
	}

	/* close the input file */
	if(fclose(input_file) == EOF)
	{
		printf("Some error occured trying to close the input file! \n");
	}

	/* close the output file */
	if(fclose(output_file) == EOF)
	{
		printf("Some error occured trying to close the output file! \n");
	}

	/* deallocate the allocated char* to avoid memory leak */
	free(line);

	/* main return value */
	return 0;
}