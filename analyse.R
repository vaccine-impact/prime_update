# File: analyse.R
# Compare burden estimates and vaccination impact from the different scenarios.

################################################################################
# 1) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2012"
# 2) unwpp_mortality = TRUE,  disability.weights = "gbd_2001", canc.inc = "2012"
# 3) unwpp_mortality = FALSE, disability.weights = "gbd_2017", canc.inc = "2012"
# 4) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2018"
# 5) unwpp_mortality = TRUE,  disability.weights = "gbd_2017", canc.inc = "2018"
################################################################################


# ------------------------------------------------------------------------------
# program start  -- load libraries, etc
# ------------------------------------------------------------------------------
program_start_analyse <- function ()
{
  # remove all objects from workspace
  remove (list = objects() )

  # start time
  print (Sys.time ())

  # load libraries
  library (data.table)    # data table
  library (ggplot2)       # graphics
  library (tictoc)
  library (countrycode)
  library (ggforce)
  library (ggpubr)
  library (rworldmap)
  library (scales)
  library (prime)

} # end of function -- program_start_analyse
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Combine burden estimates from different simulation scenarios
# Add columns for calendar year, cases, deaths, yld, yll, dalys
# Add columns for (cases, deaths, yld, yll, dalys) per 100,000
# Add column for number of vaccines administered
# ------------------------------------------------------------------------------
combine_burden_estimate <- function () {

  # simulation scenarios
  simulations = c("s1", "s2", "s3", "s4", "s5")

  # burden estimates for different simulation scenarios
  allburden <- NULL
  for (i in 1:length(simulations)) {

    # read burden estimate of one simulation scenario
    burdenfile <- paste0 ("output/", simulations[i], "_results.csv")
    burden <- fread (burdenfile, header = "auto", stringsAsFactors = F)

    # set scenario number
    burden [, simulation := simulations[i]]

    # combine burden estimate of this simulation scenario
    # to other simulation scenarios
    if (is.null(allburden)) {
      allburden <- burden
    } else {
      allburden <- rbind (allburden, burden)
    }
  }

  # set to data table
  setDT (allburden)

  # Add columns for calendar year, cases, deaths, yld, yll, dalys
  allburden [, year   := birthcohort + age]
  allburden [, cases  := cohort_size * inc.cecx]
  allburden [, deaths := cohort_size * mort.cecx]
  allburden [, yld    := cohort_size * disability]
  allburden [, yll    := cohort_size * lifey]
  allburden [, dalys  := yll + yld]

  # Add columns for (cases, deaths, yld, yll, dalys) per 100,000
  allburden [, cases_p100  := cases  / cohort_size * 100000]
  allburden [, deaths_p100 := deaths / cohort_size * 100000]
  allburden [, yld_p100    := yld    / cohort_size * 100000]
  allburden [, yll_p100    := yll    / cohort_size * 100000]
  allburden [, dalys_p100  := dalys  / cohort_size * 100000]

  # NA values result due to division by 0 for UNWPP simulations,
  # since cohort size for ages 0 to 7 are 0
  allburden [is.na(allburden)] <- 0

  # Add column for number of vaccines administered
  # vaccined administered to 9 year old girls
  allburden [(scenario=="post-vaccination" & age==9),
             vaccines := cohort_size * vaccinated, with=T]

  # return comnbined burden estimates from all simulation scenarios
  return (allburden)

} # end of function -- combine_burden_estimate
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Add WHO burden region (column)
# ------------------------------------------------------------------------------
add_WHO_region <- function (allburden) {

  # extract columns of iso3 country codes and WHO regions
  iso3_who <- prime::data.global [, .(iso3, `WHO Region`)]

  # add who region column
  allburden <- iso3_who [allburden, on = .(iso3 = country)]

  # rename column name -- who_region
  colnames(allburden) [colnames(allburden) == "WHO Region"] <- "who_region"

  # set WHO region for Palestine
  allburden [iso3 == "PSE", who_region := "EMR"]

  # rename column name -- iso3 to country
  colnames(allburden) [colnames(allburden) == "iso3"] <- "country"

  return (allburden)

} # end of function -- add_WHO_region


# ------------------------------------------------------------------------------
# plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
# plot for each country, each region and at global level
# ------------------------------------------------------------------------------
plot_cecx_burden_pre_post_vaccination <- function (allburden)
{
  # ----------------------------------------------------------------------------
  # burden comparison plot for each country
  # ----------------------------------------------------------------------------

  # plot file
  pdf ("appendix/Figure-Country_burden_pre_post_vaccination.pdf")

  # what burden to plot
  plotwhat = c("cases", "deaths", "yld", "yll", "dalys")

  y_axis <- c("Cases", "Deaths", "YLDs", "YLLs", "DALYs")

  counter <- 0
  # counter <- 174

  # loop through each country
  for (countries in unique (allburden$country)) {

    if (counter <177) {  # plot subset of countries
      counter <- counter + 1

      tic ()
      print (countries)
      country_burden <- allburden [country == countries]

      # loop through each burden metric
      for (i in 1:length (plotwhat)) {

        # burden metric
        toplot = plotwhat[i]

        print (ggplot (country_burden,
                       aes (x = birthcohort, y = get(toplot), fill=age)) +
                 geom_bar (stat="identity") +
                 scale_fill_gradientn (colours = rev(rainbow(5))) +
                 facet_grid (scenario ~ simulation, scales = "free_y") +
                 theme_bw (base_size = 8) +
                 labs (
                   x="Year of birth",
                   y=y_axis[i],
                   title = countrycode (countries, 'iso3c', 'country.name')) +
                 scale_x_continuous(breaks=seq(2011, 2020, 3)) +
                 theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                 scale_y_continuous (labels = scales::comma)
        )
      }

      toc ()
    }
  }

  dev.off ()  # close plot file

  # ----------------------------------------------------------------------------
  # burden comparison plot for each region
  # ----------------------------------------------------------------------------

  # plot file
  pdf ("appendix/Figure-WHOregion_burden_pre_post_vaccination.pdf")

  who_regions <- data.table (who_region_code = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
                             who_region_name = c("African Region",
                                                 "Region of the Americas",
                                                 "Eastern Mediterranean Region",
                                                 "European Region",
                                                 "South-East Asia Region",
                                                 "Western Pacific Region"))
  setkey (who_regions, who_region_code)

  # loop through each region
  for (region in unique (allburden$who_region)) {

    tic ()
    print (region)
    region_burden <- allburden [who_region == region]

    # apply sum function to burden columns
    # dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]
    region_burden <- region_burden [, lapply (.SD, sum),
                                    .SDcols = c ("cases", "deaths", "yld", "yll", "dalys"),
                                    by=.(age, scenario, type, simulation, birthcohort)]

    # loop through each burden metric
    for (i in 1:length (plotwhat)) {

      # burden metric
      toplot = plotwhat[i]

      print (ggplot (region_burden,
                     aes (x = birthcohort, y = get(toplot), fill=age)) +
               geom_bar (stat="identity") +
               scale_fill_gradientn (colours = rev(rainbow(5))) +
               facet_grid (scenario ~ simulation, scales = "free_y") +
               theme_bw (base_size = 8) +
               labs (
                 x="Year of birth",
                 y=y_axis[i],
                 title = who_regions [who_region_code == region, who_region_name]) +
               scale_x_continuous(breaks=seq(2011, 2020, 3)) +
               theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               scale_y_continuous (labels = scales::comma)
      )
    }

    toc ()
  }

  dev.off ()  # close plot file


  # ----------------------------------------------------------------------------
  # burden comparison plot at the global level
  # ----------------------------------------------------------------------------

  # plot file
  pdf ("results/Figure-Global_burden_pre_post_vaccination.pdf")

  # apply sum function to burden columns
  # dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]
  global_burden <- allburden [, lapply (.SD, sum),
                              .SDcols = c ("cases", "deaths", "yld", "yll", "dalys"),
                              by=.(age, scenario, type, simulation, birthcohort)]

  # loop through each burden metric
  for (i in 1:length (plotwhat)) {
    toplot = plotwhat[i]

    print (ggplot (global_burden,
                   aes (x = birthcohort, y = get(toplot), fill=age)) +
             geom_bar (stat="identity") +
             scale_fill_gradientn (colours=rev(rainbow(5))) +
             facet_grid (scenario ~ simulation) +
             theme_bw (base_size = 8) +
             labs (
               x="Year of birth",
               y=y_axis[i]) +
             scale_x_continuous(breaks=seq(2011, 2020, 3)) +
             theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
             scale_y_continuous (labels = scales::comma)
    )
  }

  dev.off ()


  # ----------------------------------------------------------------------------
  # same plot as above but in 2 figures
  # fig1 -- "cases", "deaths"
  # fig2 -- "yld",  "yll",  "dalys"

  for (j in 1:2) {

    # # figure files
    # png (paste0 ("figures/fig", j, ".png", sep=""),
    #      units="in", width=6, height=9, res=300)

    # figure files
    if (j == 1) {

      png ("figures/Figure-Global_lifetime_burden_pre_post_vaccination_cases_deaths.png",
           units="in", width=6, height=9, res=300)

    } else if (j == 2) {

      png ("figures/Figure-Global_lifetime_burden_pre_post_vaccination_ylds_ylls_dalys.png",
           units="in", width=6, height=9, res=300)
    }

    plot_title <- c ("Lifetime burden of cervical cancer (cases, deaths) caused by HPV 16/18 pre- and post-vaccination",
                     "Lifetime burden of cervical cancer (YLDs, YLLs, YLDs) caused by HPV 16/18 pre- and post-vaccination")

    # 1 plot for cases, deaths (and) another plot for ylds, ylls, dalys
    if (j == 1) {
      plotwhat <- c("cases", "deaths")
      y_axis   <- c("Cases", "Deaths")
    } else {
      plotwhat <- c("yld",  "yll",  "dalys")
      y_axis   <- c("YLDs", "YLLs", "DALYs")
    }

    plot_list <- lapply (1:length(plotwhat), function (i) {
      toplot <- plotwhat [i]

      p <- ggplot (global_burden,
                   aes (x = birthcohort, y = get(toplot), fill=age)) +
        geom_bar (stat="identity") +
        scale_fill_gradientn(colours=rev(rainbow(5))) +
        facet_grid(scenario ~ simulation, scales = "free_y") +
        theme_bw (base_size = 10) +
        labs (
          x="Year of birth",
          y=y_axis[i]) +
        scale_x_continuous(breaks=seq(2011, 2020, 3)) +
        # theme_minimal () +
        theme (axis.text.x = element_text(size=6)) +
        theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous (labels = scales::comma)
    })

    # arrange plots in a single page
    q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3)

    print (
      annotate_figure (q,
                       top = text_grob (plot_title [j],
                                        color = "black",
                                        size = 9)))

    # save figure file
    dev.off ()
  }

  return ()  # return null

} # end of function -- plot_cecx_burden_pre_post_vaccination
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# create table of country-specific cervical cancer burden
# ------------------------------------------------------------------------------
create_table_country_burden <- function (allburden) {

  # extract burden for pre-vaccination and post-vaccination
  pre_vac  <- allburden [scenario == "pre-vaccination"]
  post_vac <- allburden [scenario == "post-vaccination"]

  # extract columns for country, simulation scenarios,
  # cases, deaths, yld, yll, dalys
  pre_vac  <- pre_vac  [, list (country, simulation, cases, deaths, yld, yll, dalys)]
  post_vac <- post_vac [, list (country, simulation, cases, deaths, yld, yll, dalys)]

  # burden columns
  burden_columns <- c("cases", "deaths", "yld", "yll", "dalys")

  # summarise burden by country and simulation scenario
  #   dt[, lapply(.SD, sum, na.rm=TRUE), by=category, .SDcols=c("a", "c", "z") ]
  pre_vac <- pre_vac [, lapply (.SD, sum, na.rm=TRUE),
                      .SDcols = burden_columns,
                      by = .(country, simulation) ]

  post_vac <- post_vac [, lapply (.SD, sum, na.rm=TRUE),
                        .SDcols = burden_columns,
                        by = .(country, simulation) ]

  # sort by country and simulation scenario
  pre_vac  <- pre_vac  [order (country, simulation)]
  post_vac <- post_vac [order (country, simulation)]

  # add pre-vaccination / post-vaccination to burden column names
  setnames (pre_vac,
            old = c("simulation", "cases", "deaths", "yld", "yll", "dalys"),
            new = c("Scenario",
                    "Cases (pre-vaccination)",
                    "Deaths (pre-vaccination)",
                    "YLDs (pre-vaccination)",
                    "YLLs (pre-vaccination)",
                    "DALYs (pre-vaccination)"))

  setnames (post_vac,
            old = c("simulation", "cases", "deaths", "yld", "yll", "dalys"),
            new = c("Scenario",
                    "Cases (post-vaccination)",
                    "Deaths (post-vaccination)",
                    "YLDs (post-vaccination)",
                    "YLLs (post-vaccination)",
                    "DALYs (post-vaccination)"))

  # combine tables -- pre-vaccination (and) post-vaccination
  burden <- pre_vac [post_vac, on = .(country = country, Scenario = Scenario)]

  # add country name from iso3 country code
  burden  [, Country := countrycode (burden  [, country], origin = "iso3c", destination = "country.name")]

  # add column -- who_region
  burden <- add_WHO_region (burden)

  # update column names
  setnames (burden,
            old = c ("country",              "who_region"),
            new = c ("Country code (ISO 3)", "WHO region"))

  # set Country column as first column
  setcolorder (burden,  "Country")

  # save burden data table
  fwrite (burden,
          "appendix/Table-Cervical_cancer_burden_HPV_16_18.csv",
          col.names = T, row.names = F)

  return ()  # return null

} # end of function -- create_table_country_burden
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# create and save 2 global vaccine impact tables:
# (i) Global estimates of HPV vaccination impact
# Number of girls needed to be vaccinated to prevent cervical cancer caused by HPV 16/18
# ------------------------------------------------------------------------------
save_global_vaccine_impact_tables <- function (vaccine_impact) {

  # make a copy of global vaccine impact
  global_impact <- copy (vaccine_impact)

  # extract specific columns of interest
  # simulation scenario and cervical cancer burden averted per 1000 vaccinated girls
  global_impact <- global_impact [, .(simulation,
                                      cases_averted_perVG,
                                      deaths_averted_perVG,
                                      yld_averted_perVG,
                                      yll_averted_perVG,
                                      dalys_averted_perVG)]

  # compute -- Number of girls needed to be vaccinated to prevent cervical cancer caused by HPV 16/18
  global_impact [, numvac_prevent_case  := 1000 / cases_averted_perVG]
  global_impact [, numvac_prevent_death := 1000 / deaths_averted_perVG]
  global_impact [, numvac_prevent_yld   := 1000 / yld_averted_perVG]
  global_impact [, numvac_prevent_yll   := 1000 / yll_averted_perVG]
  global_impact [, numvac_prevent_daly  := 1000 / dalys_averted_perVG]

  # save tables of global vaccination impact
  fwrite (global_impact,
          "results/Table-Global_vaccination_impact.csv",
          col.names = T, row.names = F)

  # burden averted per 1000 vaccinated girls (round off to 2 decimal points)
  fwrite (global_impact [, lapply(.SD, round, 2),
                         .SDcols = c("cases_averted_perVG",
                                     "deaths_averted_perVG",
                                     "yld_averted_perVG",
                                     "yll_averted_perVG",
                                     "dalys_averted_perVG"),
                         by = .(simulation)],
          "tables/Table-Global_vaccination_impact_burden_averted.csv",
          col.names = T, row.names = F)

  # number of girls needed to be vaccinated to prevent
  # 1 case / 1 death / 1 yld / 1 yll / 1 daly (round off to 2 decimal points)
  fwrite (global_impact [, lapply(.SD, round, 2),
                         .SDcols = c("numvac_prevent_case",
                                     "numvac_prevent_death",
                                     "numvac_prevent_yld",
                                     "numvac_prevent_yll",
                                     "numvac_prevent_daly"),
                         by = .(simulation)],
          "tables/Table-Global_vaccination_impact_numvac_prevent_burden.csv",
          col.names = T, row.names = F)


} # end of function -- save_global_vaccine_impact_tables
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# compute vaccine impact and comparison metrics -- global level
# ------------------------------------------------------------------------------
compute_vaccine_impact <- function (allburden) {

  # burden summary
  burden_summary <- allburden [ , .(total_cases  = sum (cases),
                                    total_deaths = sum (deaths),
                                    total_yld    = sum (yld),
                                    total_yll    = sum (yll),
                                    total_dalys  = sum (dalys)),
                                # total_cohort_size = sum (cohort_size)),
                                by=.(simulation, scenario)]

  # cohort size of 9 year old girls
  burden_9 <- allburden [age == 9, .(total_cohort_size_9 = sum (cohort_size),
                                     total_vaccines      = sum (vaccines)),
                         by=.(simulation, scenario)]

  # combine burden summary tables
  burden_summary <-
    burden_summary [burden_9, on = .(simulation=simulation, scenario=scenario)]

  # compute metrics per 100,000 9-year old girls
  burden_summary [, `:=` (cases_p100  = total_cases  / total_cohort_size_9 * 100000,
                          deaths_p100 = total_deaths / total_cohort_size_9 * 100000,
                          yld_p100    = total_yld    / total_cohort_size_9 * 100000,
                          yll_p100    = total_yll    / total_cohort_size_9 * 100000,
                          dalys_p100  = total_dalys  / total_cohort_size_9 * 100000)]

  # compute vaccine impact table
  burden_summary_prevac  <- burden_summary [scenario == "pre-vaccination"]
  burden_summary_postvac <- burden_summary [scenario == "post-vaccination"]

  vaccine_impact <- burden_summary_prevac [burden_summary_postvac,
                                           on = .(simulation = simulation)]

  # compute vaccine impact -- burden averted
  vaccine_impact [, `:=` (cases_averted  = total_cases  - i.total_cases,
                          deaths_averted = total_deaths - i.total_deaths,
                          yld_averted    = total_yld    - i.total_yld,
                          yll_averted    = total_yll    - i.total_yll,
                          dalys_averted  = total_dalys  - i.total_dalys)]


  vaccine_impact [, `:=` (cases_averted_perVG  = cases_averted  / i.total_vaccines * 1000,
                          deaths_averted_perVG = deaths_averted / i.total_vaccines * 1000,
                          yld_averted_perVG    = yld_averted    / i.total_vaccines * 1000,
                          yll_averted_perVG    = yll_averted    / i.total_vaccines * 1000,
                          dalys_averted_perVG  = dalys_averted  / i.total_vaccines * 1000)]

  # create and save 2 global vaccine impact tables:
  # (i) Global estimates of HPV vaccination impact
  # Number of girls needed to be vaccinated to prevent cervical cancer caused by HPV 16/18
  save_global_vaccine_impact_tables (vaccine_impact)

  total <- c("cases_averted_perVG",
             "deaths_averted_perVG",
             "yld_averted_perVG",
             "yll_averted_perVG",
             "dalys_averted_perVG")

  for (i in total) {
    denominator <- vaccine_impact [simulation=="s1", .(get(i))]
    vaccine_impact [, paste0(i, "_p") := get(i) / denominator [, V1]]
  }

  # plot vaccine impact
  # plot lifetime health impact per 1000 vaccinated girls

  plotwhat <- c("cases_averted_perVG",
                "deaths_averted_perVG",
                "yld_averted_perVG",
                "yll_averted_perVG",
                "dalys_averted_perVG")

  y_axis <- c("Cases averted",
              "Deaths averted",
              "YLDs averted",
              "YLLs averted",
              "DALYs averted")

  plot_list = list ()

  plot_list <- lapply (1:length(plotwhat), function (i) {

    toplot = plotwhat[i]

    p <- ggplot (vaccine_impact,
                 # aes (x = simulation, y = get(toplot), fill= - get (toplot))) +
                 aes (x = simulation, y = get(toplot), fill= toplot)) +
      geom_bar (stat="identity") +
      labs (
        x = "Scenario",
        y = y_axis[i]
      ) +
      theme_bw (base_size = 10) +
      # theme_minimal() +
      theme(legend.position="none") +
      theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous (labels = scales::comma)
  })

  # arrange plot columns and rows
  q <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3)

  ggsave ("figures/Figure-Global_vaccine_impact.png",
          annotate_figure(q, top = text_grob("Lifetime health impact per 1000 vaccinated girls (global level)", color = "black", size = 12)),
          width=5, height=7.5, dpi=300)

  # compute proportions (total: cases, deaths, yld, yll, dalys) with respect to simulation s1
  total <- c("total_cases", "total_deaths", "total_yld", "total_yll", "total_dalys",
             "cases_p100", "deaths_p100", "yld_p100", "yll_p100", "dalys_p100")
  for (i in total) {

    denominator <- burden_summary [simulation=="s1", .(scenario, get(i) )]

    burden_summary [scenario=="pre-vaccination",
                    paste0(i, "_p") := get(i) / denominator [scenario=="pre-vaccination", V2]]
    burden_summary [scenario=="post-vaccination",
                    paste0(i, "_p") := get(i) / denominator [scenario=="post-vaccination", V2]]
  }

} # end of function -- compute_vaccine_impact
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# compute vaccine impact and comparison metrics -- regional level
# ------------------------------------------------------------------------------
compute_vaccine_impact_regional <- function (allburden) {

  # burden summary
  burden_summary <- allburden [ , .(total_cases  = sum (cases),
                                    total_deaths = sum (deaths),
                                    total_yld    = sum (yld),
                                    total_yll    = sum (yll),
                                    total_dalys  = sum (dalys)),
                                # total_cohort_size = sum (cohort_size)),
                                by=.(simulation, scenario, who_region)]

  # sort by who_region and simulation scenario
  burden_summary  <- burden_summary  [order (who_region, simulation)]

  # set who_region column as first column
  setcolorder (burden_summary,  "who_region")

  # cohort size of 9 year old girls
  burden_9 <- allburden [age == 9, .(total_cohort_size_9 = sum (cohort_size),
                                     total_vaccines      = sum (vaccines)),
                         by=.(simulation, scenario, who_region)]

  # combine burden summary tables
  burden_summary <-
    burden_summary [burden_9, on = .(simulation=simulation, scenario=scenario, who_region=who_region)]

  # sort by who_region and simulation scenario
  burden_summary  <- burden_summary  [order (who_region, simulation)]

  # compute metrics per 100,000 9-year old girls
  burden_summary [, `:=` (cases_p100  = total_cases  / total_cohort_size_9 * 100000,
                          deaths_p100 = total_deaths / total_cohort_size_9 * 100000,
                          yld_p100    = total_yld    / total_cohort_size_9 * 100000,
                          yll_p100    = total_yll    / total_cohort_size_9 * 100000,
                          dalys_p100  = total_dalys  / total_cohort_size_9 * 100000)]

  # compute vaccine impact table
  burden_summary_prevac  <- burden_summary [scenario == "pre-vaccination"]
  burden_summary_postvac <- burden_summary [scenario == "post-vaccination"]

  vaccine_impact <- burden_summary_prevac [burden_summary_postvac,
                                           on = .(simulation = simulation, who_region=who_region)]

  # compute vaccine impact -- burden averted
  vaccine_impact [, `:=` (cases_averted  = total_cases  - i.total_cases,
                          deaths_averted = total_deaths - i.total_deaths,
                          yld_averted    = total_yld    - i.total_yld,
                          yll_averted    = total_yll    - i.total_yll,
                          dalys_averted  = total_dalys  - i.total_dalys)]


  vaccine_impact [, `:=` (cases_averted_perVG  = cases_averted  / i.total_vaccines * 1000,
                          deaths_averted_perVG = deaths_averted / i.total_vaccines * 1000,
                          yld_averted_perVG    = yld_averted    / i.total_vaccines * 1000,
                          yll_averted_perVG    = yll_averted    / i.total_vaccines * 1000,
                          dalys_averted_perVG  = dalys_averted  / i.total_vaccines * 1000)]

  # plot vaccine impact
  # plot lifetime health impact per 1000 vaccinated girls

  who_regions <- data.table (who_region_code = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
                             who_region_name = c("African Region",
                                                 "Region of the Americas",
                                                 "Eastern Mediterranean Region",
                                                 "European Region",
                                                 "South-East Asia Region",
                                                 "Western Pacific Region"))
  setkey (who_regions, who_region_code)

  # ----------------------------------------------------------------------------
  # plot file -- cases, deaths, ylls, ylds & dalys for 5 scenarios in 6 WHO regions (6 pages)
  pdf ("appendix/Figure-WHOregion_vaccine_impact.pdf")

  counter <- 0

  # loop through each region
  for (regions in unique (vaccine_impact$who_region)) {

    if (counter <6) {  # plot subset of regions
      counter <- counter + 1

      tic ()
      print (regions)
      region_vaccine_impact <- vaccine_impact [who_region == regions]

      plotwhat <- c("cases_averted_perVG",
                    "deaths_averted_perVG",
                    "yld_averted_perVG",
                    "yll_averted_perVG",
                    "dalys_averted_perVG")

      y_axis <- c("Cases averted",
                  "Deaths averted",
                  "YLDs averted",
                  "YLLs averted",
                  "DALYs averted")

      plot_list = list ()

      plot_list <- lapply (1:length(plotwhat), function (i) {

        toplot = plotwhat[i]

        p <- ggplot (region_vaccine_impact,
                     aes (x = simulation, y = get(toplot), fill=toplot)) +
          geom_bar (stat="identity") +
          labs (
            x = "Scenario",
            y = y_axis[i]
          ) +
          theme_bw (base_size = 10) +
          theme(legend.position="none") +
          theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          scale_y_continuous (labels = scales::comma)
      })

      # arrange plot columns and rows
      q <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3)

      print (annotate_figure(q,
                             top = text_grob (paste0("Lifetime health impact per 1000 vaccinated girls - ",
                                                     who_regions [who_region_code == regions, who_region_name]),
                                              # countrycode (countries, 'iso3c', 'country.name')),
                                              color = "black", size = 12)))

    }
    toc ()

  }  # end of for loop

  dev.off ()
  # ----------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # plot file -- cases, deaths, & dalys for updated scenario (s5) in 6 WHO regions (1 page)
  # pdf ("figures/Figure-WHOregion_updated_vaccine_impact.pdf")
  # png ("figures/Figure-WHOregion_updated_vaccine_impact.png",
  #      units="in", width=6, height=3, res=900)

  who_regions <- data.table (who_region_code = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
                             who_region_name = c("African Region",
                                                 "Region of the Americas",
                                                 "Eastern Mediterranean Region",
                                                 "European Region",
                                                 "South-East Asia Region",
                                                 "Western Pacific Region"))

  # extract vaccine impact results for updated scenario (s5)
  region_vaccine_impact <- vaccine_impact [simulation == "s5"]

  # combine burden summary tables
  region_vaccine_impact <-
    who_regions [region_vaccine_impact, on = .(who_region_code=who_region)]

  # rename column name -- who_region
  colnames(region_vaccine_impact) [colnames(region_vaccine_impact) == "who_region_code"] <- "who_region"


  plotwhat <- c("cases_averted_perVG",
                "deaths_averted_perVG",
                "dalys_averted_perVG")

  y_axis <- c("Cases averted",
              "Deaths averted",
              "DALYs averted")

  plot_list = list ()

  plot_list <- lapply (1:length(plotwhat), function (i) {

    # which burden to plot
    toplot = plotwhat[i]

    # sort by burden value
    # region_vaccine_impact <- setorderv (region_vaccine_impact, cols = toplot, order = -1)

    p <- ggplot (region_vaccine_impact,
                 # aes (x = reorder (who_region_name, - dalys_averted_perVG), y = get(toplot), fill=toplot)) +
                 aes (x = reorder (who_region_name, - get(toplot)), y = get(toplot), fill= -get(toplot))) +
      geom_bar (stat="identity") +
      labs (
        # x = "WHO region",
        x = "",
        y = y_axis[i]
      ) +
      theme_bw (base_size = 8) +
      theme(legend.position="none") +
      theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous (labels = scales::comma) +
      rotate_x_text(angle = 45)
  })

  # arrange plot columns and rows
  q <- ggarrange(plotlist=plot_list, ncol = 3, nrow = 1)

  # print (annotate_figure(q,
  #                        top = text_grob ("Lifetime health impact per 1000 vaccinated girls (regional level)",
  #                                         # countrycode (countries, 'iso3c', 'country.name')),
  #                                         color = "black", size = 12)))

  ggsave (filename = "Figure-WHOregion_updated_vaccine_impact.png",
          path = "figures/",
          plot = annotate_figure(q,
                          top = text_grob ("Lifetime health impact per 1000 vaccinated girls (regional level)",
                                           # countrycode (countries, 'iso3c', 'country.name')),
                                           color = "black", size = 12)),
          units="in", width=6, height=3, dpi=300)


  # dev.off ()
  # ----------------------------------------------------------------------------

  # save streamlined table of vaccine impact
  # who_region, simulation, (cases, deaths, ylds, ylls, dalys) averted per 1000 FVG
  vaccine_impact_table <- vaccine_impact [, c("who_region", "simulation",
                                              "cases_averted_perVG",
                                              "deaths_averted_perVG",
                                              "yld_averted_perVG",
                                              "yll_averted_perVG",
                                              "dalys_averted_perVG")]

  # save a copy -- vaccine impact data table of only updated simulation scenario
  fwrite (vaccine_impact_table [simulation == "s5"],
          "results/Table-Vaccine_impact_s5_region.csv",
          col.names = T, row.names = F)

  # set who_region column as first column
  setcolorder (vaccine_impact_table,  "who_region")

  # sort by who_region
  vaccine_impact_table  <- vaccine_impact_table  [order (who_region)]

  # round off values (2 decimal points)
  vaccine_impact_table <- vaccine_impact_table [, lapply(.SD, round, 2),
                                                .SDcols = c("cases_averted_perVG",
                                                            "deaths_averted_perVG",
                                                            "yld_averted_perVG",
                                                            "yll_averted_perVG",
                                                            "dalys_averted_perVG"),
                                                by = .(who_region, simulation)]

  # update column names for burden averted
  setnames (vaccine_impact_table,
            old = c("simulation",
                    "cases_averted_perVG",
                    "deaths_averted_perVG",
                    "yld_averted_perVG",
                    "yll_averted_perVG",
                    "dalys_averted_perVG"),
            new = c("Scenario",
                    "Cases averted per 1000 vaccinated girls",
                    "Deaths averted per 1000 vaccinated girls",
                    "YLDs averted per 1000 vaccinated girls",
                    "YLLs averted per 1000 vaccinated girls",
                    "DALYs averted per 1000 vaccinated girls"))


  # save vaccine impact data table
  fwrite (vaccine_impact_table,
          "results/Table-Vaccine_impact_region.csv",
          col.names = T, row.names = F)

  # save save vaccine impact data table of only updated simulation scenario
  vaccine_impact_table_s5 <- vaccine_impact_table [Scenario == "s5"]

  # drop Scenario column
  vaccine_impact_table_s5 [, Scenario := NULL]

  # round off values (no decimal point)
  vaccine_impact_table_s5 <-
    vaccine_impact_table_s5 [, lapply(.SD, round, 0),
                             .SDcols = c(
                               "Cases averted per 1000 vaccinated girls",
                               "Deaths averted per 1000 vaccinated girls",
                               "YLDs averted per 1000 vaccinated girls",
                               "YLLs averted per 1000 vaccinated girls",
                               "DALYs averted per 1000 vaccinated girls"),
                             by = .(who_region)]

  # save save vaccine impact data table of only updated simulation scenario
  fwrite (vaccine_impact_table_s5 [order (-`DALYs averted per 1000 vaccinated girls`)],
          "tables/Table-Vaccine_impact_region_PRIME_update.csv",
          col.names = T, row.names = F)

  return ()

} # end of function -- compute_vaccine_impact_regional
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# compute vaccine impact and comparison metrics -- country level
# ------------------------------------------------------------------------------
compute_vaccine_impact_country <- function (allburden) {

  # burden summary
  burden_summary <- allburden [ , .(total_cases  = sum (cases),
                                    total_deaths = sum (deaths),
                                    total_yld    = sum (yld),
                                    total_yll    = sum (yll),
                                    total_dalys  = sum (dalys)),
                                # total_cohort_size = sum (cohort_size)),
                                by=.(simulation, scenario, country)]

  # sort by country and simulation scenario
  burden_summary  <- burden_summary  [order (country, simulation)]

  # set country column as first column
  setcolorder (burden_summary,  "country")

  # cohort size of 9 year old girls
  burden_9 <- allburden [age == 9, .(total_cohort_size_9 = sum (cohort_size),
                                     total_vaccines      = sum (vaccines)),
                         by=.(simulation, scenario, country)]

  # combine burden summary tables
  burden_summary <-
    burden_summary [burden_9, on = .(simulation=simulation, scenario=scenario, country=country)]

  # sort by country and simulation scenario
  burden_summary  <- burden_summary  [order (country, simulation)]

  # compute metrics per 100,000 9-year old girls
  burden_summary [, `:=` (cases_p100  = total_cases  / total_cohort_size_9 * 100000,
                          deaths_p100 = total_deaths / total_cohort_size_9 * 100000,
                          yld_p100    = total_yld    / total_cohort_size_9 * 100000,
                          yll_p100    = total_yll    / total_cohort_size_9 * 100000,
                          dalys_p100  = total_dalys  / total_cohort_size_9 * 100000)]

  # compute vaccine impact table
  burden_summary_prevac  <- burden_summary [scenario == "pre-vaccination"]
  burden_summary_postvac <- burden_summary [scenario == "post-vaccination"]

  vaccine_impact <- burden_summary_prevac [burden_summary_postvac,
                                           on = .(simulation = simulation, country=country)]

  # compute vaccine impact -- burden averted
  vaccine_impact [, `:=` (cases_averted  = total_cases  - i.total_cases,
                          deaths_averted = total_deaths - i.total_deaths,
                          yld_averted    = total_yld    - i.total_yld,
                          yll_averted    = total_yll    - i.total_yll,
                          dalys_averted  = total_dalys  - i.total_dalys)]


  vaccine_impact [, `:=` (cases_averted_perVG  = cases_averted  / i.total_vaccines * 1000,
                          deaths_averted_perVG = deaths_averted / i.total_vaccines * 1000,
                          yld_averted_perVG    = yld_averted    / i.total_vaccines * 1000,
                          yll_averted_perVG    = yll_averted    / i.total_vaccines * 1000,
                          dalys_averted_perVG  = dalys_averted  / i.total_vaccines * 1000)]

  # ----------------------------------------------------------------------------
  # plot vaccine impact
  # plot lifetime health impact per 1000 vaccinated girls
  # plot file -- cases, deaths, ylls, ylds & dalys for 5 scenarios in 177 countries (177 pages)
  pdf ("appendix/Figure-Country_vaccine_impact.pdf")

  counter <- 0
  # counter <- 174

  # loop through each country
  for (countries in unique (vaccine_impact$country)) {

    if (counter <177) {  # plot subset of countries
      counter <- counter + 1

      tic ()
      print (countries)
      country_vaccine_impact <- vaccine_impact [country == countries]

      plotwhat <- c("cases_averted_perVG",
                    "deaths_averted_perVG",
                    "yld_averted_perVG",
                    "yll_averted_perVG",
                    "dalys_averted_perVG")

      y_axis <- c("Cases averted",
                  "Deaths averted",
                  "YLDs averted",
                  "YLLs averted",
                  "DALYs averted")

      plot_list = list ()

      plot_list <- lapply (1:length(plotwhat), function (i) {

        toplot = plotwhat[i]

        p <- ggplot (country_vaccine_impact,
                     aes (x = simulation, y = get(toplot), fill=toplot)) +
          geom_bar (stat="identity") +
          labs (
            x = "Scenario",
            y = y_axis[i]
          ) +
          theme_bw (base_size = 10) +
          theme(legend.position="none") +
          theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          scale_y_continuous (labels = scales::comma)
      })

      # arrange plot columns and rows
      q <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3)

      print (annotate_figure(q,
                             top = text_grob (paste0("Lifetime health impact per 1000 vaccinated girls - ",
                                                     countrycode (countries, 'iso3c', 'country.name')),
                                              color = "black", size = 12)))

    }
    toc ()

  }  # end of for loop

  dev.off ()
  # ----------------------------------------------------------------------------


  # ----------------------------------------------------------------------------
  # plot file -- cases, deaths & dalys for updated scenario (s5) in 177 countries (1 page)
  pdf ("appendix/Figure-Country_comparison_vaccine_impact.pdf")

  # extract vaccine impact results for updated scenario (s5)
  country_vaccine_impact <- vaccine_impact [simulation == "s5"]

  # add country name from iso3 country code
  country_vaccine_impact [, Country := countrycode (country_vaccine_impact  [, country],
                                                    origin = "iso3c",
                                                    destination = "country.name")]

  plotwhat <- c("cases_averted_perVG",
                "deaths_averted_perVG",
                "yld_averted_perVG",
                "yll_averted_perVG",
                "dalys_averted_perVG")

  y_axis <- c("Cases averted",
              "Deaths averted",
              "YLDs averted",
              "YLLs averted",
              "DALYs averted")

  plot_list = list ()

  plot_list <- lapply (1:length(plotwhat), function (i) {

    # which burden to plot
    toplot = plotwhat[i]

    # p <- ggplot (country_vaccine_impact,
    print (ggplot (country_vaccine_impact,
                   aes (x = reorder (Country, get(toplot)), y = get(toplot), fill = get(toplot))) +
             geom_bar (stat="identity") +
             labs (
               x = NULL,
               y = y_axis[i],
               title = paste0 (y_axis[i], " per 1000 vaccinated girls")
             ) +
             theme_bw (base_size = 8) +
             theme(legend.position="none") +
             theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
             scale_y_continuous (labels = scales::comma) +
             scale_fill_gradientn(colours = rev(terrain.colors(10))) +
             coord_flip() +
             theme (axis.text.y=element_text(size=rel(0.5)))

    )
  })

  dev.off ()
  # ----------------------------------------------------------------------------

  # save streamlined table of vaccine impact
  # country, simulation, (cases, deaths, ylds, ylls, dalys) averted per 1000 FVG
  vaccine_impact_table <- vaccine_impact [, c("country", "simulation",
                                              "cases_averted_perVG",
                                              "deaths_averted_perVG",
                                              "yld_averted_perVG",
                                              "yll_averted_perVG",
                                              "dalys_averted_perVG")]

  # save a copy -- vaccine impact data table of only updated simulation scenario
  vaccine_impact_table <- add_WHO_region (vaccine_impact_table)
  fwrite (vaccine_impact_table [simulation == "s5"],
          "results/Table-Vaccine_impact_s5_iso3.csv",
          col.names = T, row.names = F)

  # add country name from iso3 country code
  vaccine_impact_table [, Country := countrycode (vaccine_impact_table  [, country],
                                                  origin = "iso3c",
                                                  destination = "country.name")]

  # set Country column as first column
  setcolorder (vaccine_impact_table,  "Country")

  # sort by Country name
  vaccine_impact_table  <- vaccine_impact_table  [order (Country)]

  # vaccine impact table (shorter column names)
  vaccine_impact_tab <- copy (vaccine_impact_table)

  # update column names for burden averted
  setnames (vaccine_impact_table,
            old = c("country",
                    "who_region",
                    "simulation",
                    "cases_averted_perVG",
                    "deaths_averted_perVG",
                    "yld_averted_perVG",
                    "yll_averted_perVG",
                    "dalys_averted_perVG"),
            new = c("Country code (ISO 3)",
                    "WHO region",
                    "Scenario",
                    "Cases averted per 1000 vaccinated girls",
                    "Deaths averted per 1000 vaccinated girls",
                    "YLDs averted per 1000 vaccinated girls",
                    "YLLs averted per 1000 vaccinated girls",
                    "DALYs averted per 1000 vaccinated girls"))

  # save vaccine impact data table
  fwrite (vaccine_impact_table,
          "appendix/Table-Vaccine_impact.csv",
          col.names = T, row.names = F)

  # ----------------------------------------------------------------------------
  # vaccine impact data table of only updated simulation scenario (s5)
  vaccine_impact_table_s5 <- vaccine_impact_table [Scenario == "s5"]

  # drop Scenario column
  vaccine_impact_table_s5 [, Scenario := NULL]

  # round off values (no decimal point)
  vaccine_impact_table_s5 <-
    vaccine_impact_table_s5 [, lapply(.SD, round, 0),
                             .SDcols = c(
                               "Cases averted per 1000 vaccinated girls",
                               "Deaths averted per 1000 vaccinated girls",
                               "YLDs averted per 1000 vaccinated girls",
                               "YLLs averted per 1000 vaccinated girls",
                               "DALYs averted per 1000 vaccinated girls"),
                             by = .(Country)]

  # save save vaccine impact data table of only updated simulation scenario
  fwrite (vaccine_impact_table_s5,
          "tables/Table-Vaccine_impact_country_PRIME_update.csv",
          col.names = T, row.names = F)

  # return vaccine impact table (shorter column names)
  return (vaccine_impact_tab)

} # end of function -- compute_vaccine_impact_country
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# vaccine impact -- country and scenario comparison
# ------------------------------------------------------------------------------
compare_vaccine_impact_country_scenario <- function (vaccine_impact_tab) {

  # ----------------------------------------------------------------------------
  # Which countries change the most from the update --
  # in terms of cases/deaths/dalys averted per 1000 vaccinated girls,
  # in % terms from the previous estimates
  # --> ( (s5 - s1) / s1 ) * 100

  # vaccine impact data table of base simulation scenario (s1)
  vaccine_impact_table_s1 <- vaccine_impact_tab [simulation == "s1"]

  # plot file -- percentage change in scenarios (s2, s3, s4, s5) in comparison
  # to base scenario s1 -- cases, deaths, yll, yld & dalys averted per 1000 FVG
  pdf ("appendix/Figure-Country_comparison_vaccine_impact_scenarios.pdf")

  for (scenarios in c("s2", "s3", "s4", "s5")) {

    # vaccine impact data table of only updated simulation scenario (s5)
    vaccine_impact_table_s5 <- vaccine_impact_tab [simulation == scenarios]

    # merge tables to compare scenarios -- s1 and s5
    # dat3=merge(dat1,dat2,by="ID") merge two tables by ID field
    vaccine_impact_s1_s5 <- merge (vaccine_impact_table_s1,
                                   vaccine_impact_table_s5,
                                   by="country")

    # compute % change in burden averted between s1 and s5 scenarios
    # --> ( (s5 - s1) / s1 ) * 100
    vaccine_impact_s1_s5 [, cases_averted_ratio := ((cases_averted_perVG.y -
                                                       cases_averted_perVG.x) /
                                                      cases_averted_perVG.x) * 100]

    vaccine_impact_s1_s5 [, deaths_averted_ratio := ((deaths_averted_perVG.y -
                                                        deaths_averted_perVG.x) /
                                                       deaths_averted_perVG.x) * 100]

    vaccine_impact_s1_s5 [, yld_averted_ratio := ((yld_averted_perVG.y -
                                                     yld_averted_perVG.x) /
                                                    yld_averted_perVG.x) * 100]

    vaccine_impact_s1_s5 [, yll_averted_ratio := ((yll_averted_perVG.y -
                                                     yll_averted_perVG.x) /
                                                    yll_averted_perVG.x) * 100]

    vaccine_impact_s1_s5 [, dalys_averted_ratio := ((dalys_averted_perVG.y -
                                                       dalys_averted_perVG.x) /
                                                      dalys_averted_perVG.x) * 100]


    # ----------------------------------------------------------------------------

    plotwhat <- c("cases_averted_ratio",
                  "deaths_averted_ratio",
                  "yld_averted_ratio",
                  "yll_averted_ratio",
                  "dalys_averted_ratio")

    y_axis <- c("Percentage change in cases averted",
                "Percentage change in deaths averted",
                "Percentage change in YLDs averted",
                "Percentage change in YLLs averted",
                "Percentage change in DALYs averted")


    # drop rows for Qatar and UAE (Globocan 2012 burden data ~ 0)
    vaccine_impact_s1_s5 <- vaccine_impact_s1_s5 [country != "QAT" & country != "ARE"]

    plot_list = list ()

    plot_list <- lapply (1:length(plotwhat), function (i) {

      # which burden to plot
      toplot = plotwhat[i]

      print (ggplot (data = vaccine_impact_s1_s5,
                     aes (x = reorder (Country.x, get(toplot)), y = get(toplot))) +
               geom_col (aes (fill = get(toplot)))  +
               labs (
                 # x = "Country",
                 x = NULL,
                 y = paste0 (y_axis[i], " per 1000 vaccinated girls"),
                 title = paste0 (y_axis[i], " per 1000 vaccinated girls"),
                 subtitle = paste0 ("Comparsion of scenario ", scenarios,
                                    " in comparison to scenario s1")
               )  +
               theme_bw (base_size = 8) +
               theme(legend.position="none") +
               theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               scale_y_continuous (labels = scales::comma) +
               scale_colour_gradient2(low = muted("red"), mid = "white",
                                      high = muted("blue"), midpoint = 0) +
               coord_flip() +
               theme (axis.text.y=element_text(size=rel(0.5))) +
               scale_fill_gradientn(colours = rev(terrain.colors(10)))
      )

    })

  }

  dev.off ()
  # ----------------------------------------------------------------------------

  return ()

} # end of function -- compare_vaccine_impact_country_scenario
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# start of program
program_start_analyse ()  # load libraries, etc

# Combine burden estimates from different simulation scenarios
allburden <- combine_burden_estimate ()

# Add WHO burden region (column)
allburden <- add_WHO_region (allburden)

# plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
# plot for each country and at global level
plot_cecx_burden_pre_post_vaccination (allburden)

# create table of country-specific cervical cancer burden
create_table_country_burden (allburden)

# compute vaccine impact -- global level
compute_vaccine_impact (allburden)

# compute vaccine impact -- regional level
compute_vaccine_impact_regional (allburden)

# compute vaccine impact -- country level
vaccine_impact_tab <- compute_vaccine_impact_country (allburden)

# vaccine impact -- country and scenario comparison
compare_vaccine_impact_country_scenario (vaccine_impact_tab)


# ------------------------------------------------------------------------------
print (Sys.time ())
# end of program
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
