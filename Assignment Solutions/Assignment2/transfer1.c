#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <string.h>
#include <semaphore.h>
#include <unistd.h>


#define BUFFER_SIZE 128
char *buffer;
sem_t bufferSemaphore;


void *drainThread(void *outfile) {
  char *f = (char *) outfile;
  FILE *out;
  int outptr;

  if ((out = fopen(f, "w")) == NULL) {
    fprintf(stderr, "could not open output file %s\n", f);
    exit(1);
  }

  outptr = 0;
  do {
    char local[BUFFER_SIZE];

    sem_wait(&bufferSemaphore);
    if (buffer[outptr] != 0) {
      strcpy(local, &buffer[outptr]);
      printf("drain thread: read [%s] from buffer\n", local);
      if (!strcmp(local, "QUIT")) {
	return NULL;
      } else {
	if (fwrite(local, 1, strlen(local), out) != strlen(local))
	  fprintf(stderr, "failed to write line %s to output\n", local), exit(1);
      }
      outptr = outptr + strlen(local) + 1;
    } else {
      printf("drain thread: no new string in buffer\n");
    }
    sem_post(&bufferSemaphore);
  } while (1);

  return NULL;
}


struct args_t {
  char *file;
  useconds_t usecs;
};

void *fillThread(void *args) {
  struct args_t *a = (struct args_t *) args;
  char *f = a->file;
  useconds_t sleepTime = a->usecs;
  FILE *in;
  ssize_t nread;
  int inptr;

  if ((in = fopen(f, "r")) == NULL) {
    fprintf(stderr, "could not open input file %s\n", f);
    exit(1);
  }

  inptr = 0;
  do {
    char *line;
    size_t ignored;

    line = NULL;
    nread = getline(&line, &ignored, in);
    sem_wait(&bufferSemaphore);
    if (nread != -1) {
      strcpy(&buffer[inptr], line);
      inptr = inptr + nread + 1;
      printf("fill thread: wrote [%s] into buffer\n", line);
      free(line);
    } else {
      strcpy(&buffer[inptr], "QUIT");
      inptr = inptr + 5;
      printf("fill thread: wrote QUIT into buffer\n");
    }
    sem_post(&bufferSemaphore);
    usleep(sleepTime);
  } while (nread != -1);

  (void) fclose(in);
  return NULL;
}


int main(int argc, char *argv[]) {
  int rc;
  pthread_t tid;
  struct args_t fillArgs;

/* check command line arguments */
  if (argc != 4) {
    fprintf(stderr, "usage: %s infile outfile sleeptime\n", argv[0]);
    exit(1);
  }

/* initialize shared buffer */
  if ((rc = sem_init(&bufferSemaphore, 0, 1)) < 0)
    fprintf(stderr, "semaphore init failed: %s\n", strerror(rc)), exit(1);
  if ((buffer = calloc(BUFFER_SIZE, 1)) == NULL)
    fprintf(stderr, "buffer allocation failed\n"), exit(1);

/* start thread that takes from buffer & writes to output file */
  if ((rc = pthread_create(&tid, NULL, drainThread, argv[2])) != 0)
    fprintf(stderr, "thread create failed (%s)\n", strerror(rc)), exit(1);

/* this thread reads from input file & puts into buffer */
  fillArgs.file = argv[1];
  fillArgs.usecs = atoi(argv[3]);
  (void) fillThread((void *)&fillArgs);

/* wait for other thread to terminate then free resources */
  (void) pthread_join(tid, NULL);
  (void) sem_destroy(&bufferSemaphore);
  free(buffer);

  return 0;
}
