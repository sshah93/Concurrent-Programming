/*	
*	Suketu Shah
*	CS-511-A
*	Assignment 2 - Part 1
*/

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char**argv)
{
	FILE* input_file;
	FILE* output_file;
	char* line;
	size_t line_length;
	ssize_t bytes_read;

	line_length = 10;
	line = (char*) malloc(line_length + 1);
	
	if(line == NULL)
	{
		printf("Some error occured while trying to allocate memory using function malloc! \n");
		exit(1);
	}

	if(argc != 3)
	{
		printf("Usage: ./rw <input file> <output file> \n");
		exit(1);
	}

	input_file = fopen(argv[1], "r");

	if(input_file == NULL)
	{
		printf("Couldn't open the input file for reading \n");
		exit(1);
	}

	output_file = fopen(argv[2], "w");

	if(output_file == NULL)
	{
		printf("Output file couldn't be open to write to the file \n");
		exit(1);
	}

	while((bytes_read = getline(&line, &line_length, input_file)) != -1)
	{
		fwrite(line, sizeof(char), bytes_read, output_file);
	}

	if(fclose(input_file) == EOF)
	{
		printf("Some error occured trying to close the input file! \n");
	}

	if(fclose(output_file) == EOF)
	{
		printf("Some error occured trying to close the output file! \n");
	}

	free(line);

	return 0;
}
