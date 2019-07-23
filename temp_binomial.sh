#!/bin/bash -l
#$ -l h_rt=72:00:00
#$ -l buyin
#$ -N temp_binomial
#$ -pe omp 4
#$ -o /usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/qsub_output
module load R
Rscript /usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/geo_BionomProbYearTemp.R