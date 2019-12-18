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
# different scenarios from cecx burden estimates pre- and post-vaccination with 
# 4vHPV / 9vHPV at different ages
estimate_all_cecx_burden <- function (scenario, 
                                      vaccine, 
                                      vaccination_age) {
  
  # input file cecx burden estimates (vaccine HPV types) pre- and post-vaccination
  cecx_burden_file <- paste0 ("output/", scenario, 
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
  # cecx_burden_prepostvac <- cecx_burden_prepostvac [hpv_distribution, 
  #                                                   on = .(country = iso3)]
  
  cecx_burden_prepostvac <- merge (x     = cecx_burden_prepostvac, 
                                               y     = hpv_distribution,
                                               by.x  = "country",
                                               by.y  = "iso3", 
                                               all.x = TRUE)
  
  
  
  return (cecx_burden_prepostvac)
  
} # end of function -- estimate_all_cecx_burden


# ------------------------------------------------------------------------------
# start of program
print (Sys.time ())
# ------------------------------------------------------------------------------

# simulation scenarios
simulations = c("s1", "s2", "s3", "s4", "s5")

# add 3 loops (ater) -- scenario, vaccine, vaccination_age
scenario <- scenarios [1] 
vaccine <- vaccines [1]
vaccination_age <- vaccination_ages [1]


# Estimate all cecx burden estimates pre- and post-vaccination for 
# different scenarios from cecx burden estimates pre- and post-vaccination with 
# 4vHPV / 9vHPV at different ages
all_cecx_burden <- estimate_all_cecx_burden (scenario        = scenario, 
                                             vaccine         = vaccine, 
                                             vaccination_age = vaccination_age)



# ------------------------------------------------------------------------------
print (Sys.time ())
# end of program
# ------------------------------------------------------------------------------
