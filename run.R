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

# vaccination age and HPV vaccine type
vaccination_ages <- c (9, 12)
vaccines         <- c ("4vHPV", "9vHPV")
# vaccination_ages <- c (9)
# vaccines         <- c ("4vHPV")

# generate burden estimates for all 5 scenarios
# source ("batchrun.R")  #### UNCOMMENT this line for full run

# compare burden estimates and vaccination impact from the 5 scenarios
# source ("analyse.R")  #### UNCOMMENT this line for full run

# create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination
# source ("map.R")  #### UNCOMMENT this line for full run

# Estimate cervical cancer burden estimates for different scenarios.
# (not just the cervical cancer burden attributable to HPV types contained
# in the vaccine)
# source ("estimate_full_cecx_burden.R")  #### UNCOMMENT this line for full run

toc ()
