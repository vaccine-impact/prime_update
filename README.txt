File: README.txt

This folder contains files for analysing different comparative scenarios and 
assess the health impact estimates in the PRIME model. 

--------------------------------------------------------------------------------
Scenarios | Demography   | Disability weights | Cervical cancer burden
--------------------------------------------------------------------------------
s1        | WHO (2009)   | GBD 2001           | GLOBOCAN 2012
s2        | UNWPP (2017) | GBD 2001           | GLOBOCAN 2012
s3        | WHO (2009)   | GBD 2017           | GLOBOCAN 2012
s4        | WHO (2009)   | GBD 2001           | GLOBOCAN 2018
s5        | UNWPP (2017) | GBD 2017           | GLOBOCAN 2018
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
Files:
- run.R
    Run file for PRIME modelling analysis for effects of demography, disability 
    weights and cervical cancer burden on HPV vaccination impact estimates in 
    177 countries.
    By running this 1 file, complete analysis can be done and results generated.

- batchrun.R 
    Generate burden estimates for different scenarios

- analyse.R
    Compare burden estimates and vaccination impact from the different scenarios

- map.R
    create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination

- estimate_full_cecx_burden.R
    Compare burden estimates for all HPV types and comparing between vaccines (4vPHV and 9vHPV)

- combine_figures_tables.R
    Combine figures and tables for paper appendix
--------------------------------------------------------------------------------    
Folders:

- output
    Output files of disease burden estimates for different scenarios.
      (related to HPV types contained in the vaccine)
    
- output_all
    Output files of disease burden estimates for different scenarios.
      (related to all HPV types, and not just the HPV types contained in the vaccine)

- results
    Plots and tables of results

- results_all
    Plots at global level for all HPV types and comparing between vaccines (4vPHV and 9vHPV)
    
- maps
    Global maps of country-level vaccine impact estimates
    
- appendix
    Figures (pdf files) and tables for paper (appendix)
    
- figures
    Figures in the paper (main part)

- figures_all
    Figures in the paper (main part) for all HPV types and comparing between vaccines (4vPHV and 9vHPV)
    
- tables
    Tables in the paper (main part) 

- paper_results
    Results (numbers) for the paper

--------------------------------------------------------------------------------

