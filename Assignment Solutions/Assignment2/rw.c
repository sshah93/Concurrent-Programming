#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


int main(int argc, char *argv[]) {
  FILE *in, *out;
#define MAXLINE 128
  char *buffer = calloc(MAXLINE, 1);
  int nread;

  if (argc != 3) {
    fprintf(stderr, "usage: %s infile outfile\n", argv[0]);
    exit(1);
  }

  if ((in = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "could not open input file %s\n", argv[1]);
    exit(1);
  }
  if ((out = fopen(argv[2], "w")) == NULL) {
    fprintf(stderr, "could not open output file %s\n", argv[2]);
    exit(1);
  }

  do {
    size_t ignored;

    nread = getline(&buffer, &ignored, in);
    if (nread != -1)
      if (fwrite(buffer, 1, nread, out) != nread)
	fprintf(stderr, "failed to write line %s to output\n", buffer), exit(1);
  } while (nread != -1);

  return 0;
}
