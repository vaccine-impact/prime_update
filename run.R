# File: run.R

# Run file for PRIME modelling analysis for effects of demography, disability 
# weights and cervical cancer burden on HPV vaccination impact estimates in 
# 177 countries

# generate burden estimates for all 5 scenarios
source ("batchrun.R")

# compare burden estimates and vaccination impact from the 5 scenarios
source ("analyse.R")

# create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination
source ("map.R")