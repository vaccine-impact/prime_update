# File: run.R

# Run file for PRIME modelling analysis for effects of demography, disability 
# weights and cervical cancer burden on HPV vaccination impact estimates in 
# 177 countries

library (tictoc)
library (data.table)    # data table
library (ggplot2)       # graphics
library (tictoc)        # timing R scripts
library (foreach)       # looping -- supports parallel execution
library (doParallel)    # Foreach Parallel Adaptor for the 'parallel' Package
library (countrycode)
library (ggforce)
library (ggpubr)
library (rworldmap)
library (scales)
library (prime)         # PRIME model






# remove all objects from workspace
rm (list = ls ())

# start time
print (Sys.time ())
tic ()

# generate burden estimates for all 5 scenarios
source ("batchrun.R")

# compare burden estimates and vaccination impact from the 5 scenarios
source ("analyse.R")

# create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination
source ("map.R")

toc ()