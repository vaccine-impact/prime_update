library (pdftools)

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
combine_tables <- function () {
  
  # Table-Cervical_cancer_burden
  
  
  
  # Table-Vaccine_impact
  
  
} # end of function -- combine_tables
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Combine figures 
# appendix: combine figures (pdf files) for different vaccination ages and vaccines
combine_figures ()

# Combine tables 
# appendix: combine tables (csv files) for different vaccination ages and vaccines
combine_tables ()
# ------------------------------------------------------------------------------




