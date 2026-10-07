#!/bin/bash
#SBATCH --job-name=brms_sensitivity
#SBATCH --output=brms_sens_%j.out
#SBATCH --error=brms_sens_%j.err
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem=32G
#SBATCH --time=3:00:00

echo "Job started at: $(date)"
echo "Using $SLURM_CPUS_PER_TASK CPUs"

export MAKEFLAGS="-j $SLURM_CPUS_PER_TASK"

Rscript pmi_prior_sensitivity.R

echo "Job finished at: $(date)"
