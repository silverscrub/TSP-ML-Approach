#!/bin/bash

#This script is used to automate the clustering procedure of the algorithm. The current goal is to divide a given TSP problem into subproblems and call Concorde Solver to tackle every single one


Rscript generate_case.R
Rscript clustering.R
