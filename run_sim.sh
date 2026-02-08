#!/bin/bash
#SBATCH --array=1-18%9
#SBATCH --job-name=run_sim_job
#SBATCH --partition=wrobel
#SBATCH -o slurm-%A_%a.out
#SBATCH -e slurm-%A_%a.err

module purge
module load R/4.4.0

export R_LIBS_USER=/projects/wrobel/users/linlin/R/4.4

JOBID=$SLURM_ARRAY_TASK_ID
Rscript analysis/run_simulation.R $JOBID


