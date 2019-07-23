#!/bin/bash -l
#$ -l h_rt=72:00:00
#$ -l buyin
#$ -N long_run_binomial_null
#$ -pe omp 4
#$ -o /usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/qsub_output
module load R
Rscript /usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/geo_binomial_null_fitting.R
