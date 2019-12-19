# File: estimate_full_cecx_burden.R
# Estimate cervical cancer burden estimates for different scenarios.
# (not just the cervical cancer burden attributable to HPV types contained 
# in the vaccine)
# 
# Input files (folder: ./output )
# Cecx burden estimates pre- and post-vaccination with 4vHPV and 9vHPV. 
# Cecx burden estimates are specific to (cervical cancer causing) HPV types 
# contained in the vaccine. 
# 4vHPV - HPV 16/18
# 9vHPV - HPV 16/18 + HPV 31/33/45/52/58
# 
# Output files: (folder: ./output_all_cecx)
# All -- Cecx burden estimates pre- and post-vaccination
# (not just the cervical cancer burden attributable to HPV types contained 
# in the vaccine)

# Estimate all cecx burden estimates pre- and post-vaccination for 
# different simulation scenarios from cecx burden estimates 
# pre- and post-vaccination with 4vHPV / 9vHPV at different ages
estimate_all_cecx_burden <- function (simulation, 
                                      vaccine, 
                                      vaccination_age) {
  
  # input file cecx burden estimates (vaccine HPV types) pre- and post-vaccination
  cecx_burden_file <- paste0 ("output/", simulation, 
                              "_results_age", vaccination_age, 
                              "_", vaccine, ".csv")
  
  # read cecx burden estimates (vaccine HPV types) pre- and post-vaccination
  cecx_burden <- fread (cecx_burden_file, header = "auto", stringsAsFactors = F)
  
  # split cecx burden estimates by pre- and post-vaccination
  cecx_burden_prevac  <- cecx_burden [scenario == "pre-vaccination" ]
  cecx_burden_postvac <- cecx_burden [scenario == "post-vaccination"]
  
  # combine data tables by matched columns
  # cecx_burden_prevac & cecx_burden_postvac
  cecx_burden_prepostvac <- cecx_burden_prevac [cecx_burden_postvac, 
                                                on = .(type        = type,
                                                       age         = age, 
                                                       country     = country, 
                                                       birthcohort = birthcohort)]
  
  # create data table of country (iso3) and HPV vaccine type distribution
  if (vaccine == "4vHPV") {
    hpv_distribution <- data.hpv_distribution [, c ("iso3", "hpv_4v")]
    
    # rename hpv distribution column to hpv
    setnames (hpv_distribution, old = c("hpv_4v"), new = c ("hpv"))
    
  } else if (vaccine == "9vHPV") {
    
    hpv_distribution <- data.hpv_distribution [, c ("iso3", "hpv_9v")]
    
    # rename hpv distribution column to hpv
    setnames (hpv_distribution, old = c("hpv_9v"), new = c ("hpv"))
  }
  
  # combine data tables -- cecx_burden_prepostvac & hpv_distribution 
  cecx_burden_prepostvac <- merge (x     = cecx_burden_prepostvac, 
                                   y     = hpv_distribution,
                                   by.x  = "country",
                                   by.y  = "iso3", 
                                   all.x = TRUE)
  
  # ----------------------------------------------------------------------------
  # add additional burden due to non-vaccine hpv types causing cervical cancer to
  # both pre- and post-vaccination burden due to vaccine hpv types causing cervical cancer
  
  # incidence
  # cecx_burden_prepostvac [, i.inc.cecx := i.inc.cecx + (inc.cecx * (100/hpv - 1)) ]
  # cecx_burden_prepostvac [, inc.cecx   := inc.cecx * (100/hpv) ]
  
  # burden -- incidence, mortality, yll, yld, cost
  for (burden_type in c("inc.cecx", "mort.cecx", "lifey", "disability", "cost.cecx")) {
    
    cecx_burden_prepostvac [, paste0("i.",burden_type) := 
                              get (paste0("i.",burden_type)) + 
                              (get(burden_type) * (100/hpv - 1)) ]
    
    cecx_burden_prepostvac [, paste0(burden_type) := get(burden_type) * 100/hpv]
  }
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # (i) split the table into 2 tables for pre- and post-vaccination with burden
  #     estimates for all hpv types causing cervical cancer
  # (ii) combine the 2 tables for pre- and post-vaccination
  
  cecx_burden_prevac_all <- cecx_burden_prepostvac [, c("country", 
                                                        "scenario", 
                                                        "type", 
                                                        "age", 
                                                        "cohort_size", 
                                                        "vaccinated", 
                                                        "immunized", 
                                                        "inc.cecx", 
                                                        "mort.cecx", 
                                                        "lifey", 
                                                        "disability", 
                                                        "cost.cecx", 
                                                        "birthcohort")]
  
  cecx_burden_postvac_all <- cecx_burden_prepostvac [, c("country", 
                                                        "i.scenario", 
                                                        "type", 
                                                        "age", 
                                                        "i.cohort_size", 
                                                        "i.vaccinated", 
                                                        "i.immunized", 
                                                        "i.inc.cecx", 
                                                        "i.mort.cecx", 
                                                        "i.lifey", 
                                                        "i.disability", 
                                                        "i.cost.cecx", 
                                                        "birthcohort")]
  
  
  # rename column names
  setnames (cecx_burden_postvac_all, 
            old = c("i.scenario",
                    "i.cohort_size", 
                    "i.vaccinated", 
                    "i.immunized", 
                    "i.inc.cecx", 
                    "i.mort.cecx", 
                    "i.lifey", 
                    "i.disability", 
                    "i.cost.cecx"), 
            new = c("scenario",
                    "cohort_size", 
                    "vaccinated", 
                    "immunized", 
                    "inc.cecx", 
                    "mort.cecx", 
                    "lifey", 
                    "disability", 
                    "cost.cecx"))
  
  # combine the 2 tables for pre- and post-vaccination
  cecx_burden_all <- rbind (cecx_burden_prevac_all, 
                            cecx_burden_postvac_all, 
                            use.names = TRUE)
  # ----------------------------------------------------------------------------
  
  # save file cecx burden estimates (all HPV types) pre- and post-vaccination
  fwrite (cecx_burden_all, 
          paste0 ("output_all/", 
                  simulation, 
                  "_results_age",vaccination_age, 
                  "_", vaccine, ".csv"), 
          col.names = T, row.names = F)
  
  return () 
  
} # end of function -- estimate_all_cecx_burden


# ------------------------------------------------------------------------------
# start of program
print (Sys.time ())
# ------------------------------------------------------------------------------

# vaccination age and HPV vaccine type
vaccination_ages <- c (9, 12)
vaccines         <- c ("4vHPV", "9vHPV")

# simulation scenarios
simulations = c("s1", "s2", "s3", "s4", "s5")

# add 3 loops (ater) -- simulation scenario, vaccine, vaccination_age
for (simulation in simulations) { 
  
  for (vaccine in vaccines) {
    
    for (vaccination_age in vaccination_ages) {
      
      # Estimate all cecx burden estimates pre- and post-vaccination for 
      # different simulation scenarios from cecx burden estimates 
      # pre- and post-vaccination with 4vHPV / 9vHPV at different ages
      estimate_all_cecx_burden (simulation      = simulation, 
                                vaccine         = vaccine, 
                                vaccination_age = vaccination_age)
    }
  }
}

# ------------------------------------------------------------------------------
# get burden estimates for the different vaccines (4vHPV, 9vHPV)

# combine burden estimates from different simulation scenarios
allburden_4v <- combine_burden_estimate (vaccine         = vaccines [1],
                                         vaccination_age = vaccination_ages [1],
                                         folder          = "output_all/")

# combine burden estimates from different simulation scenarios
allburden_9v <- combine_burden_estimate (vaccine         = vaccines [2],
                                         vaccination_age = vaccination_ages [1],
                                         folder          = "output_all/")


# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
print (Sys.time ())
# end of program
# ------------------------------------------------------------------------------
