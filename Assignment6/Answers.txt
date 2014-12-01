Part 1

1a. part1.pml

1b. Erigone command line for the verification run -> erigone -s -lt1000 part1

1c. Erigone verification output that shows that n may finish outside the range 5-10 -> verification terminated=assert statement is false,line=26,statement={assert(5 <= n && n <= 10)},



Part 2

2a. part2.pml

2b. Erigone command line for the verification run -> erigone -s -lt1000 part2 

2c. Erigone verification output that shows that n may finish as 2 -> verification terminated=assert statement is false,line=26,statement={assert(n!=2)},

2d. Erigone command line for the guided simulation run -> erigone -g -dm part2

2e. File that contains guided simulation output: 2e.txt



Part 3

3a. part3.pml

3b. Erigone command line for the verification run -> erigone -s -lt1000 part3

3c. Erigone verification output that shows that n always finishes as 10 -> verification terminated=successfully,



Part 4

4a. part4.pml

4b. Erigone command line for the verification run -> erigone -s -lt100000 part4

4c. Erigone verification output that shows that all carts pass through the intersection in order -> verification terminated=successfully,