#!/bin/bash

Rscript generate_case.R

Rscript clustering.R

#automate concorde solver to solve automatically every .tsp file generated in the format

ls | grep 'branch\_[0-9]*\_case_1.tsp' > to_do.txt

filename="to_do.txt"
while read line; do
# reading each line
~/concorde/TSP/concorde -x $line
echo "$line done!"
done< $filename


ls | grep 'branch\_[0-9]*\_case_1.sol' > solution_files.txt

