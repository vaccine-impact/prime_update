# File: combine_figures_tables.R
# Combine figures and tables for paper appendix

# ------------------------------------------------------------------------------
# Combine figures 
# appendix: combine figures (pdf files) for different vaccination ages and vaccines 
# ------------------------------------------------------------------------------
combine_figures <- function () {
  
  # change to appendix sub-folder
  setwd ("./appendix")
  
  # Combine pdf files to create a joined file
  # pdf_combine (c ("file1.pdf", "file2.pdf"), output = "joined.pdf")
  
  # Figure-WHOregion_burden_pre_post_vaccination
  pdf_combine (c ("Figure-WHOregion_burden_pre_post_vaccination_age9_4vHPV.pdf", 
                  "Figure-WHOregion_burden_pre_post_vaccination_age9_9vHPV.pdf", 
                  "Figure-WHOregion_burden_pre_post_vaccination_age12_4vHPV.pdf", 
                  "Figure-WHOregion_burden_pre_post_vaccination_age12_9vHPV.pdf"), 
               output = "Figure-WHOregion_burden_pre_post_vaccination.pdf")
  
  # Figure-WHOregion_vaccine_impact
  pdf_combine (c ("Figure-WHOregion_vaccine_impact_age9_4vHPV.pdf", 
                  "Figure-WHOregion_vaccine_impact_age9_9vHPV.pdf", 
                  "Figure-WHOregion_vaccine_impact_age12_4vHPV.pdf", 
                  "Figure-WHOregion_vaccine_impact_age12_9vHPV.pdf"), 
               output = "Figure-WHOregion_vaccine_impact.pdf")
  
  # Figure_Global_maps_vaccine_impact_scenarios
  pdf_combine (c ("Figure_Global_maps_vaccine_impact_scenarios_age9_4vHPV.pdf", 
                  "Figure_Global_maps_vaccine_impact_scenarios_age9_9vHPV.pdf", 
                  "Figure_Global_maps_vaccine_impact_scenarios_age12_4vHPV.pdf", 
                  "Figure_Global_maps_vaccine_impact_scenarios_age12_9vHPV.pdf"), 
               output = "Figure_Global_maps_vaccine_impact_scenarios.pdf")
  
  # Figure-Country_comparison_vaccine_impact
  pdf_combine (c ("Figure-Country_comparison_vaccine_impact_age9_4vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_age9_9vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_age12_4vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_age12_9vHPV.pdf"), 
               output = "Figure-Country_comparison_vaccine_impact.pdf")
  
  # Figure-Country_comparison_vaccine_impact_scenarios
  pdf_combine (c ("Figure-Country_comparison_vaccine_impact_scenarios_age9_4vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_scenarios_age9_9vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_scenarios_age12_4vHPV.pdf", 
                  "Figure-Country_comparison_vaccine_impact_scenarios_age12_9vHPV.pdf"), 
               output = "Figure-Country_comparison_vaccine_impact_scenarios.pdf")

  # change back to main folder
  setwd ("../")
  
  return ()  # return null
  
} # end of function -- combine_figures
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Combine tables 
# appendix: combine tables (csv files) for different vaccination ages and vaccines 
# ------------------------------------------------------------------------------
combine_tables <- function (table_file) {
  
  # change to appendix sub-folder
  setwd ("./appendix")
  
  # read in table files for different vaccination ages and vaccines
  age9_4vHPV <- fread (paste0 (table_file, "_age9_4vHPV.csv"), 
                       header = "auto", stringsAsFactors = F)
  
  age9_9vHPV <- fread (paste0 (table_file, "_age9_9vHPV.csv"), 
                       header = "auto", stringsAsFactors = F)
  
  age12_4vHPV <- fread (paste0 (table_file, "_age12_4vHPV.csv"), 
                       header = "auto", stringsAsFactors = F)
  
  age12_9vHPV <- fread (paste0 (table_file, "_age12_9vHPV.csv"), 
                       header = "auto", stringsAsFactors = F)
  
  # add 2 columns for vaccination age and vaccine
  age9_4vHPV [, `:=` ('Vaccination age (years)' = 9, 
                      'HPV vaccine' = "bivalent/quadrivalent")]
  
  age9_9vHPV [, `:=` ('Vaccination age (years)' = 9, 
                      'HPV vaccine' = "nonavalent")]
  
  age12_4vHPV [, `:=` ('Vaccination age (years)' = 12, 
                      'HPV vaccine' = "bivalent/quadrivalent")]
  
  age12_9vHPV [, `:=` ('Vaccination age (years)' = 12, 
                      'HPV vaccine' = "nonavalent")]
  
  # column order
  setcolorder (age9_4vHPV, c ("Vaccination age (years)", "HPV vaccine") )
  setcolorder (age9_9vHPV, c ("Vaccination age (years)", "HPV vaccine") )
  setcolorder (age12_4vHPV, c ("Vaccination age (years)", "HPV vaccine") )
  setcolorder (age12_9vHPV, c ("Vaccination age (years)", "HPV vaccine") )
  
  # combine tables (CONTINUE)
  combined_table <- rbindlist (list (age9_4vHPV, age9_9vHPV, age12_4vHPV, age12_9vHPV), 
                               use.names = TRUE)
    
  # save combined table to file
  fwrite (combined_table, file = paste0 (table_file, ".csv"))
  
  # change back to main folder
  setwd ("../")
  
  
} # end of function -- combine_tables
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Combine figures 
# appendix: combine figures (pdf files) for different vaccination ages and vaccines
combine_figures ()

# Combine tables 
# appendix: combine tables (csv files) for different vaccination ages and vaccines
combine_tables (table_file = "Table-Cervical_cancer_burden")  # cervical cancer burden 
combine_tables (table_file = "Table-Vaccine_impact")          # vaccine impact
# ------------------------------------------------------------------------------




