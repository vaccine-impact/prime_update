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
# plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
# plot at global level
# Note: all burden -- all cervical cancer cases (not just caused hpv types in vaccine)
# ------------------------------------------------------------------------------
plot_all_cecx_burden_pre_post_vaccination <- function (allburden_4v,
                                                       allburden_9v,
                                                       vaccination_age) {

  # ----------------------------------------------------------------------------
  # burden comparison plot at the global level
  # novaccination, 4vHPV and 9vHPV
  # ----------------------------------------------------------------------------

  # plot file
  pdf (paste0 ("results_all/Figure-Global_all_cecx_burden_pre_post_vaccination_age",
               vaccination_age, ".pdf"))

  # what burden to plot
  plotwhat <- c("cases", "deaths", "yld", "yll", "dalys")

  y_axis   <- c("Cases", "Deaths", "YLDs", "YLLs", "DALYs")

  # ----------------------------------------------------------------------------
  # apply sum function to burden columns
  # dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]
  global_burden_4v <- allburden_4v [, lapply (.SD, sum),
                                    .SDcols = c ("cases", "deaths", "yld", "yll", "dalys"),
                                    by=.(age, scenario, type, simulation, birthcohort)]

  global_burden_9v <- allburden_9v [, lapply (.SD, sum),
                                    .SDcols = c ("cases", "deaths", "yld", "yll", "dalys"),
                                    by=.(age, scenario, type, simulation, birthcohort)]

  # ----------------------------------------------------------------------------

  # loop through vaccines -- 4vPHV and 9vHPV
  for (j in 1:2) {

    # set variables based on vaccine (1 - 4vHPV or 2 - 9VHPV)
    if (j == 1) {

      global_burden <- global_burden_4v
      vaccine_type  <- "bivalent/quadrivalent"


    } else if (j == 2) {

      global_burden <- global_burden_9v
      vaccine_type  <- "nonavalent"
    }

    # loop through each burden metric
    for (i in 1:length (plotwhat)) {

      # burden metric
      toplot = plotwhat[i]

      # plot title
      plot_title <- paste0 ("Global",
                            "\n Lifetime burden of cervical cancer ",
                            y_axis[i],
                            " pre- and post-vaccination \n (vaccination age = ",
                            vaccination_age,
                            " years / ", vaccine_type, " vaccine)")

      # plot
      print (ggplot (global_burden,
                     aes (x = birthcohort, y = get(toplot), fill=age)) +
               geom_bar (stat="identity") +
               scale_fill_gradientn (colours=rev(rainbow(5))) +
               facet_grid (scenario ~ simulation) +
               theme_bw (base_size = 8) +
               labs (
                 x="Year of birth",
                 y=y_axis[i],
                 title = plot_title) +
               scale_x_continuous(breaks=seq(2008, 2020, 3)) +
               theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               scale_y_continuous (labels = scales::comma)
      )
    }

  }
  dev.off ()


  # ----------------------------------------------------------------------------
  # same plot as above but combine 4vHPV and 9VHPV in the same plot

  # loop through simulation scenarios
  for (sim_scenario in c ("s1", "s2", "s3", "s4", "s5")) {

    # get global lifetime burden estimates for specific simulation scenario
    global_burden_4v_simulation <- global_burden_4v [simulation == sim_scenario]
    global_burden_9v_simulation <- global_burden_9v [simulation == sim_scenario]

    # add vaccine type to post-vaccination
    global_burden_4v_simulation [scenario == "post-vaccination", scenario :=
      "bivalent / quadrivalent vaccination"]
    global_burden_9v_simulation [scenario == "post-vaccination", scenario :=
                                   "nonavalent vaccination"]

    # create table of lifetime burden estimates with vaccine types -- 4vHPV & 9vHPV
    global_burden <- rbind (global_burden_4v_simulation,
                            global_burden_9v_simulation [scenario == "nonavalent vaccination"],
                            use.names = TRUE)

    # ordering for plots
    global_burden$scenario <- factor (global_burden$scenario,
                                      levels = c ("nonavalent vaccination",
                                                  "bivalent / quadrivalent vaccination",
                                                  "pre-vaccination") )

    # loop through 2 figures per simulation scenario
    # fig1 -- "cases", "deaths", "dalys"
    # fig2 -- "yld",  "yll",  "dalys"
    for (j in 1:2) {

      # figure files
      # j == 1 --> fig1 -- "cases", "deaths", "dalys"
      # j == 2 --> fig2 -- "yld",  "yll",  "dalys"
      if (j == 1) {

        png (paste0 ("figures_all/",
                     sim_scenario, "_age", vaccination_age,
                     "_Figure-Global_lifetime_all_burden_pre_post_vaccination_cases_deaths_dalys.png"),
             units="in", width=13, height=9.5, res=300)

        plotwhat <- c ("cases", "deaths", "dalys")
        y_axis   <- c ("Cases", "Deaths", "DALYs")

      } else if (j == 2) {

        png (paste0 ("figures_all/",
                     sim_scenario, "_age", vaccination_age,
                     "_Figure-Global_lifetime_all_burden_pre_post_vaccination_ylds_ylls_dalys.png"),
             units="in", width=13, height=9.5, res=300)

        plotwhat <- c ("yld",  "yll",  "dalys")
        y_axis   <- c ("YLDs", "YLLs", "DALYs")
      }

      # plot title
      plot_title <- c ("Lifetime burden of cervical cancer (cases, deaths, DALYs) pre- and post-vaccination",
                       "Lifetime burden of cervical cancer (YLDs, YLLs, DALYs) pre- and post-vaccination")


      plot_list <- lapply (1:length(plotwhat), function (i) {
        toplot <- plotwhat [i]

        p <- ggplot (global_burden,
                     aes (x = birthcohort, y = get(toplot), fill=age)) +
          geom_bar (stat="identity") +
          scale_fill_gradientn(colours=rev(rainbow(5))) +
          facet_grid(scenario ~ ., scales = "fixed") +
          theme_bw (base_size = 10) +
          labs (
            x="Year of birth",
            y=y_axis[i]) +
          scale_x_continuous(breaks=seq(2008, 2020, 3)) +
          # theme_minimal () +
          theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          scale_y_continuous (labels = scales::comma) +
          # theme (axis.text.x = element_text(size=12)) +
          theme (axis.text = element_text (size = 12)) +
          theme (strip.text.y = element_text (size = 12))  +
          theme (axis.title = element_text (size = 18)) +
          theme (legend.title = element_text (size = 15),
                 legend.text  = element_text (size = 12)
          )
      })

      # arrange plots in a single page
      q <- ggarrange (plotlist=plot_list, ncol = 3, nrow = 1)

      print (
        annotate_figure (q,
                         top = text_grob (paste0 (plot_title [j],
                                                  " \n (HPV vaccination of ",
                                                  vaccination_age,
                                                  "-year-old girls at 90% coverage)"),
                                          color = "black",
                                          size = 21)))

      # save figure file
      dev.off ()
      
      # ------------------------------------------------------------------------
      # save figure in eps format for paper
      setEPS ()
      
      # assign filename
      if (j == 1) {
        file_name <- paste0 ("figures_all/",
                             sim_scenario, "_age", vaccination_age,
                             "_Figure-Global_lifetime_all_burden_pre_post_vaccination_cases_deaths_dalys.eps")
      } else if (j == 2) {
        file_name <- paste0 ("figures_all/",
                             sim_scenario, "_age", vaccination_age,
                             "_Figure-Global_lifetime_all_burden_pre_post_vaccination_ylds_ylls_dalys.eps")
      }
      
      postscript (file = file_name, width = 13, height = 9.5)
      
      print (
        annotate_figure (q,
                         top = text_grob (paste0 (plot_title [j],
                                                  " \n (HPV vaccination of ",
                                                  vaccination_age,
                                                  "-year-old girls at 90% coverage)"),
                                          color = "black",
                                          size = 21)))
      dev.off ()
      # ------------------------------------------------------------------------
    }

  }

  return ()  # return null

} # end of function -- plot_all_cecx_burden_pre_post_vaccination
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Save country-level estimates of HPV vaccination impact for paper appendix.
# The estimated lifetime impact of HPV vaccination of adolescent girls at 
# 9-years or 12-years with bivalent/quadrivalent or nonavalent vaccines during 
# 2020-2029 at 90% coverage in 177 countries. Cases, deaths, YLLs, YLDs and DALYs 
# averted per 1000 vaccinated girls, after the combined PRIME updates for 
# demography, disability weights and cervical cancer burden.
# ------------------------------------------------------------------------------
vaccine_impact_country_PRIME_update_age9_4vHPV_9vHPV_age12_4vHPV_9vHPV <- function () {
  
  # read in vaccine impact country level tables
  age9_4vHPV <- fread (file = "tables/Table-Vaccine_impact_country_PRIME_update_age9_4vHPV.csv", 
                       header = "auto", 
                       stringsAsFactors = F)
  
  age9_9vHPV <- fread (file = "tables/Table-Vaccine_impact_country_PRIME_update_age9_9vHPV.csv", 
                       header = "auto", 
                       stringsAsFactors = F)
  
  age12_4vHPV <- fread (file = "tables/Table-Vaccine_impact_country_PRIME_update_age12_4vHPV.csv", 
                        header = "auto", 
                        stringsAsFactors = F)
  
  age12_9vHPV <- fread (file = "tables/Table-Vaccine_impact_country_PRIME_update_age12_9vHPV.csv", 
                        header = "auto", 
                        stringsAsFactors = F)
  
  # combine tables
  age9_4vHPV_9vHPV <- age9_4vHPV [age9_9vHPV,
                                  on = .(Country = Country)]
  
  age12_4vHPV_9vHPV <- age12_4vHPV [age12_9vHPV,
                                    on = .(Country = Country)]
  
  age9_4vHPV_9vHPV_age12_4vHPV_9vHPV <- age9_4vHPV_9vHPV [age12_4vHPV_9vHPV, 
                                                          on = .(Country = Country)]
  
  
  # save full vaccine impact country table (single table)
  # The estimated lifetime impact of HPV vaccination of adolescent girls at 
  # 9-years or 12-years with bivalent/quadrivalent or nonavalent vaccines during 
  # 2020-2029 at 90% coverage in 177 countries.
  fwrite (age9_4vHPV_9vHPV_age12_4vHPV_9vHPV,
          file = "tables/Table-Vaccine_impact_country_PRIME_update_age9_4vHPV_9vHPV_age12_4vHPV_9vHPV.csv",
          col.names = T, 
          row.names = F)
  
  return ()  # return null
  
} # end of function -- vaccine_impact_country_PRIME_update_age9_4vHPV_9vHPV_age12_4vHPV_9vHPV
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# start of program
print (Sys.time ())
# ------------------------------------------------------------------------------

# vaccination age and HPV vaccine type
# (this is set up in run.R)
# vaccination_ages <- c (9, 12)
# vaccines         <- c ("4vHPV", "9vHPV")

# simulation scenarios
simulations = c("s1", "s2", "s3", "s4", "s5")

# ------------------------------------------------------------------------------
# Estimate all cecx burden estimates pre- and post-vaccination for
# different simulation scenarios from cecx burden estimates
# pre- and post-vaccination with 4vHPV / 9vHPV at different ages

# 3 loops -- simulation scenario, vaccine, vaccination_age
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


# ------------------------------------------------------------------------------
# get burden estimates for the different vaccines (4vHPV, 9vHPV)

# things to do -- start loop here for vaccination ages 9y, 12y
for (vaccination_age in vaccination_ages) {

  # combine burden estimates from different simulation scenarios for "4vHPV"
  allburden_4v <- combine_burden_estimate (vaccine         = vaccines [1],
                                           vaccination_age = vaccination_age,
                                           folder          = "output_all/")

  # combine burden estimates from different simulation scenarios for "9vHPV"
  allburden_9v <- combine_burden_estimate (vaccine         = vaccines [2],
                                           vaccination_age = vaccination_age,
                                           folder          = "output_all/")

  # plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
  # plot at global level
  # Note: all burden -- all cervical cancer cases (not just caused hpv types in vaccine)
  plot_all_cecx_burden_pre_post_vaccination (allburden_4v,
                                             allburden_9v,
                                             vaccination_age = vaccination_age)

}
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Save country-level estimates of HPV vaccination impact for paper appendix.
vaccine_impact_country_PRIME_update_age9_4vHPV_9vHPV_age12_4vHPV_9vHPV ()
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
print (Sys.time ())
# end of program
# ------------------------------------------------------------------------------
