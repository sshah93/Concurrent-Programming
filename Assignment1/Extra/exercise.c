//In order to use threads, include the thread library by running  by putting in the compile line:
//-lpthread

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define MAX_THREADS 8

char *greetings[MAX_THREADS];

void *start(void *args)
{
	printf("thread 
}

int main(int argc, char *argv[]) {
	int i;
	int rc;
	pthread_t tid;
    greetings[0] = "English: Hello World!";
    greetings[1] = "French: Bonjour, le monde!";
    greetings[2] = "Spanish: Hola al mundo";
    greetings[3] = "Klingon: Nuq neH!";
    greetings[4] = "German: Guten Tag, Welt!"; 
    greetings[5] = "Russian: Zdravstvytye, mir!";
    greetings[6] = "Japan: Sekai e konnichiwa!";
    greetings[7] = "Latin: Orbis, te saluto!";

    if (argc != 2) {
        fprintf(stderr, "usage: %s num\n", argv[0]);
        exit(0);
    }
	
    num_threads = atoi(argv[1]);
    
	if (num_threads > MAX_THREADS)
        exit(0);

    for(i=0l i<num_threads;i++)
		rc = pthead_create(&tid, NULL, start, NULL);
		if(rc != 0)
		{
			fprintf(stderr, "create failed with error\n", argv[0]);
			exit(0);
		}

    return 0;
}