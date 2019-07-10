README

This folder contains files for analysing different comparative scenarios and 
assess the health impact estimates in the PRIME model. 

-----------------------------------------------------------------------
Scenarios | Demography   | Disability weights | Cervical cancer burden
-----------------------------------------------------------------------
s1        | WHO (2009)   | GBD 2001           | GLOBOCAN 2012
s2        | UNWPP (2017) | GBD 2001           | GLOBOCAN 2012
s3        | WHO (2009)   | GBD 2017           | GLOBOCAN 2012
s4        | WHO (2009)   | GBD 2001           | GLOBOCAN 2018
s5        | UNWPP (2017) | GBD 2017           | GLOBOCAN 2018
-----------------------------------------------------------------------


Files:
- batchrun.R 
    Create batch of cohorts with information on 
    countries, vaccination year, vaccination age and vaccination coverage, and
    simulates the different comarative scenarios for these birth cohorts
    and estimate/compare vaccination impact.

- analyse.R
    Compare burden estimates and vaccination impact from the different scenarios.

Folders:
- input
  Input files (vaccine coverage & disease burden template)
- results
  Output files of disease burden estimates
- plots
  Plots to visualise results

