#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>



struct process_info {
  int pid;
  int fd;
};
struct process_info *child;
int num_children;
#define CLEAN_EXIT 0x2B



/*
 *  Return 1/0 if argument is/isn't prime.
 *  Code taken from http://c2.com/cgi/wiki?SieveOfEratosthenes
 */
int sieve(int top_value) {
  int count = top_value - 1;
  int *array = calloc(top_value + 1, sizeof(int));
  int i, prime, multiple;

/* mark each int as potentially prime */
  for (i=2; i <= top_value; ++i) 
    array[i] = 1;

/* for each starting prime, mark its every multiple as non-prime */
  for (prime = 2; prime <= top_value; ++prime) {
    if (array[prime])
      for (multiple = 2*prime; multiple <= top_value; multiple += prime)
	if (array[multiple]) {
	  array[multiple] = 0;
	  --count;
	}
  }

/* return 1 if top_value is prime, 0 if it's not */
  return array[top_value];
}



/*
 *  Find all primes in the range [bottom, top].
 *  Write each prime into the pipe.
 *  Close pipe when done to indicate to parent that we're done.
 */
void find_primes(int bottom, int top, int pipefd) {
  int i, isprime;

  printf("child %i: bottom=%i, top=%i\n", getpid(), bottom, top);
  for (i=bottom; i<=top; i++) {
    isprime = sieve(i);
    if (isprime)
      write(pipefd, &i, sizeof(i));
  }
  close(pipefd);
}



/*
 *  Create a new child process and set up a pipe from it into parent.
 *  Child will search the range [bottom, top].
 *  Child's pid and pipe fd are stored in child[proc].
 */
void create_child(int proc, int bottom, int top) {
  int pid;
  int fds[2];

  if (pipe(fds) < 0) {
    perror("pipe failed");
    exit(1);
  }
  if ((pid = fork()) < 0) {
    perror("fork failed");
    exit(1);
  } else if (pid == 0) {
    close(fds[0]);                       /* form pipe */
    find_primes(bottom, top, fds[1]);    /* child finds primes */
    exit(CLEAN_EXIT);
  } else {
    close(fds[1]);                       /* form pipe */
    child[proc].pid = pid;               /* parent remembers children */
    child[proc].fd = fds[0];
  }
}



/*
 *  return "highest_fd" and "readset" based on which children are active
 */
void set_readfds(int *highest_fd, fd_set *readset) {
  int i, n, fd;

  n = -1;
  FD_ZERO(readset);
  for (i=0; i<num_children; i++) {
    if ((fd = child[i].fd) != -1) {
      FD_SET(fd, readset);
      if (fd > n)
	n = fd;
    }
  }
  *highest_fd = n+1;

  return;
}

/*
 *  return number of child that is using file desc "fd"
 */
int get_child_num(int fd) {
  int i;
  for (i=0; i<num_children; i++)
    if (child[i].fd == fd)
      return i;
  return -1;
}


/*
 *  For each child process:
 *  - read & display primes reported by child
 *  - close pipe when child is done reporting
 *  - wait for child exit & report if child exited cleanly
 */
void get_children() {
  int i, rc, retval, status;

  int living_children = num_children;    /* num_children must remain unchanged */
  while (living_children > 0) {
    int nfds, nready;
    fd_set readfds;
    
    set_readfds(&nfds, &readfds);
    nready = select(nfds, &readfds, NULL, NULL, NULL);
    while (nready > 0) {
      for (i=0; i<nfds; i++) {
	if (FD_ISSET(i, &readfds)) {     /* file descriptor i is ready */
	  rc = read(i, &retval, sizeof(int));
	  if (rc > 0) {                  /* child reports a prime */
	    printf("%i is prime\n", retval);
	  } else if (rc == 0) {          /* child closed pipe */
	    int dead;
	    if ((dead = get_child_num(i)) > -1) {
	      close(child[dead].fd);
	      child[dead].fd = -1;
	      living_children--;
	      waitpid(child[dead].pid, &status, 0);
	      if (WEXITSTATUS(status) == CLEAN_EXIT)
		printf("child %i exited cleanly\n", child[dead].pid);
	    }
	  } else {
	    perror("read failed"), exit(1);
	  }
	  nready--;
	}
      }
    }
  }

}



int main(int argc, char *argv[]) {
  int top, bottom;
  int i;

/* assume command line arguments are positive integers in increasing order */
  if (argc == 1) {
    fprintf(stderr, "usage: %s <increasing positive integers>\n", argv[0]);
    exit(1);
  }
  num_children = argc - 1;
  child = calloc(num_children, sizeof(struct process_info));

/* create child process for each range */
  bottom = 2;
  i = 1;
  while (i < argc) {
    top = atoi(argv[i]);
    create_child(i-1, bottom, top);
    bottom = top + 1;
    i++;
  }

/* get/display children's results & shut down children */
  get_children();

  return 0;
}
