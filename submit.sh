#!/bin/bash
 
 #---------------------------------------------------------------------------------
 # Account information
 
#SBATCH --account=staff         # basic (default), phd, faculty, pi-<account>
 
 #---------------------------------------------------------------------------------
 # Resources requested

#SBATCH --partition=standard       # standard (default), long, gpu, mpi, highmem
#SBATCH --cpus-per-task=4          # number of CPUs requested (for parallel tasks)
#SBATCH --mem=16G           # requested memory
#SBATCH --time=06-00:00:00          # wall clock limit (d-hh:mm:ss)

#---------------------------------------------------------------------------------
# Job specific name (helps organize and track progress of jobs)

#SBATCH --job-name=malcolmKiltsJob    # user-defined job name

#---------------------------------------------------------------------------------
# Print some useful variables

echo "Job ID: $SLURM_JOB_ID"
echo "Job User: $SLURM_JOB_USER"
echo "Num Cores: $SLURM_JOB_CPUS_PER_NODE"

#---------------------------------------------------------------------------------
# Load necessary modules for the job

module load  R/4.3/4.3.2

#---------------------------------------------------------------------------------
# Commands to execute below...

Rscript kiltsYearSorter.r