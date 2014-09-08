#include "header.h"

#define PIDS_SIZE 200
#define BUFFER_SIZE 1

typedef enum 
{
	false, 
	true
} 	bool; 

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
}