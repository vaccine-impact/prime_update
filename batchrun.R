# File: batchrun.R
# Create batch of cohorts with information on
# countries, vaccination year, vaccination age and vaccination coverage,
# and simulates the different comarative scenarios for these birth cohorts
# and estimate/compare vaccination impact.

################################################################################
#
# Compare scenarios
# s1. lifetable (who), disability weight (gbd 2001), globocan (2012)  -- BASE
# s2. lifetable <<(unwpp)>>, disability weight (gbd 2001), globocan (2012)
# s3. lifetable (who), <<disability weight (gbd 2017)>>, globocan (2012)
# s4. lifetable (who), disability weight (gbd 2001), <<globocan (2018)>>
# s5. lifetable (unwpp)>>, <<disability weight (gbd 2017)>>, <<globocan (2018)>>
#
# create lifetables based on UNWPP mx estimates or WHO data
# unwpp_mortality = TRUE / FALSE
#
# disability weights from GBD (2001 or 2017)
# disability.weights = gbd_2001 / gbd_2017
#
# cervical cancer data from Globocan (2012 or 2018)
# canc.inc = 2012 / 2018
#
################################################################################


#-------------------------------------------------------------------------------
# create batch file for vaccination of cohorts
# ------------------------------------------------------------------------------
batch_cohort_vaccination <- function (vaccination_age) {

  # initialise an empty batch input table
  batch_total <- data.table (country_code = character (),
                             year         = numeric (),
                             age_first    = numeric (),
                             age_last     = numeric (),
                             coverage     = numeric () )

  # cohorts for a single country
  batch_country <- data.table (year      = 2020:2029,
                               age_first = vaccination_age,
                               age_last  = vaccination_age,
                               coverage  = 0.9)


  # create comple batch input/info file for all countries
  # # for (iso3_code in data.incidence$iso3) {
  for (iso3_code in data.incidence [Year == 2018, iso3]) {
  # for (iso3_code in data.incidence [Year == 2018, iso3] [1:1]) {


    # exclude countries for which runs don't work -- check later # debug
    if ((iso3_code != "GLP") & (iso3_code != "REU") & (iso3_code != "NCL") &
        (iso3_code != "MTQ") & (iso3_code != "PYF") & (iso3_code != "GUM") &
        (iso3_code != "PRI") & (iso3_code != "GUF") )
    {
      # create cohorts for a specific country
      batch_single <- batch_country
      batch_single [, country_code := iso3_code]

      # add to full set of cohorts for all countries
      batch_total <- rbindlist (list (batch_total, batch_single),
                                use.names = T)
    }
  }

  # return batch cohorts
  return (batch_total)

} # end of function -- batch_cohort_vaccination
# ------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# run batch file for vaccination of cohorts and estimate vaccination impact
# ------------------------------------------------------------------------------
estimate_vaccine_impact <- function (vaccine,
                                     vaccination_age) {

  ##############################################################################
# 1) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2012"
# 2) unwpp_mortality = TRUE,  disability.weights = "gbd_2001", canc.inc = "2012"
# 3) unwpp_mortality = FALSE, disability.weights = "gbd_2017", canc.inc = "2012"
# 4) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2018"
# 5) unwpp_mortality = TRUE,  disability.weights = "gbd_2017", canc.inc = "2018"
  ##############################################################################

  # run 5 scenarios
  for (i in 1:5) {

    print (paste0 ("scenario = ", i))  # debug -- remove later

    tic ()  # track simulation time -- start

    # set inputs for different scenarios
    if (i == 1) {unwpp <- F; dw <- "gbd_2001"; canc <- "2012"}
    if (i == 2) {unwpp <- T; dw <- "gbd_2001"; canc <- "2012"}
    if (i == 3) {unwpp <- F; dw <- "gbd_2017"; canc <- "2012"}
    if (i == 4) {unwpp <- F; dw <- "gbd_2001"; canc <- "2018"}
    if (i == 5) {unwpp <- T; dw <- "gbd_2017"; canc <- "2018"}

    ################### updated runs
    results <- BatchRun(countries                       = -1,
                        coverage                        = -1,
                        agevac                          = -1,
                        agecohort                       = -1,
                        sens                            = -1,
                        year_born                       = -1,
                        year_vac                        = -1,
                        runs                            = 1,
                        vaccine_efficacy_beforesexdebut = 1,
                        vaccine_efficacy_aftersexdebut  = 0,
                        log                             = -1,
                        by_calendaryear                 = FALSE,
                        use_proportions                 = TRUE,
                        analyseCosts                    = FALSE,
                        psa                             = 0,
                        psa_vals                        = ".data.batch.psa",
                        unwpp_mortality                 = unwpp,
                        disability.weights              = dw,
                        canc.inc                        = canc,
                        vaccine                         = vaccine
    )

    ################################################################################

    # save full results
    results_file <- paste0 ("output/s",
                            i,
                            "_results_age",
                            vaccination_age,
                            "_",
                            vaccine,
                            ".csv")
    fwrite (results, results_file)

    toc ()  # track simulation time -- stop
  }

} # end of function -- estimate_vaccine_impact
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# start of program
print (Sys.time ())
# ------------------------------------------------------------------------------

# loop through vaccination ages
for (vaccination_age in vaccination_ages) {

  # loop through vaccines
  for (vaccine in vaccines) {

    # create batch of cohorts with information on
    # countries, vaccination year, vaccination age, vaccination coverage
    batch_cohorts <- batch_cohort_vaccination (vaccination_age = vaccination_age)

    # print (batch_cohorts)  # testing/debug remove later

    # register batch cohorts
    RegisterBatchData (batch_cohorts, force = T)

    cl <- makeCluster (detectCores())   # registering number of cores
    registerDoParallel (cl)             # start of parallelisation

    # run batch file for vaccination of cohorts and estimate vaccination impact
    estimate_vaccine_impact (vaccine         = vaccine,
                             vaccination_age = vaccination_age)

    stopCluster (cl)                    # end of parallelisation
  }
}

#-------------------------------------------------------------------------------
print (Sys.time ())
# end of program
#-------------------------------------------------------------------------------

