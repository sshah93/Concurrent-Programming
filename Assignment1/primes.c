#include "header.h"

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
	
	int fd;
	int pid;
}