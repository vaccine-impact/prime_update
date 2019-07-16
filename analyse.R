# File: analyse.R
# Compare burden estimates and vaccination impact from the different scenarios.

################################################################################
# 1) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2012"
# 2) unwpp_mortality = TRUE,  disability.weights = "gbd_2001", canc.inc = "2012"
# 3) unwpp_mortality = FALSE, disability.weights = "gbd_2017", canc.inc = "2012"
# 4) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2018"
# 5) unwpp_mortality = TRUE,  disability.weights = "gbd_2017", canc.inc = "2018"
################################################################################

#-------------------------------------------------------------------------------
# program start  -- load libraries, etc
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
  
} # end of function -- program_start_analyse


#-------------------------------------------------------------------------------
# Combine burden estimates from different simulation scenarios
# Add columns for calendar year, cases, deaths, yld, yll, dalys
# Add columns for (cases, deaths, yld, yll, dalys) per 100,000
# Add column for number of vaccines administered
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


# plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
# plot for each country and at global level
plot_cecx_burden_pre_post_vaccination <- function (allburden)
{
  
  # ----------------------------------------------------------------------------
  # burden comparison plot for each country
  
  # plot file
  pdf ("results/Figure-country_burden_pre_post_vaccination.pdf")
  
  # what burden to plot
  plotwhat = c("cases", "deaths", "yld", "yll", "dalys")
  # "cohort_size", 
  # "cases_p100", "deaths_p100", "yld_p100", "yll_p100", "dalys_p100")
  
  y_axis <- c("Cases", "Deaths", "YLDs", "YLLs", "DALYs")
  
  counter <- 0
  
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
                 facet_grid (scenario ~ simulation) +
                 theme_bw (base_size = 8) +
                 labs (
                   x="Year of birth",
                   y=y_axis[i],
                   title = countrycode (countries, 'iso3c', 'country.name')) +
                 scale_x_continuous(breaks=seq(2011, 2020, 3))
        )
      }
      
      toc ()
    }
  }
  
  dev.off ()  # close plot file
  
  # ----------------------------------------------------------------------------
  # burden comparison plot at the global level
  
  # plot file 
  pdf ("results/Figure-global_burden_pre_post_vaccination.pdf")
  
  # copy all burden data table
  gburden <- copy (allburden)
  
  # set country column to NULL (drop country column)
  gburden [, country := NULL]
  
  # apply sum function to burden columns
  global_burden <- gburden [, lapply (.SD, sum), 
                .SDcols = c ("cases", "deaths", "yld", "yll", "dalys"), 
                by=.(age, scenario, type, simulation, birthcohort)]
  
  # global_burden <- gburden [, lapply (.SD, sum), by=.(age, scenario, type, simulation, birthcohort)]
  # dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]
  


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
             scale_x_continuous(breaks=seq(2011, 2020, 3))
    )
  }
  
  dev.off ()
  
  
  # ------------------------------------------------------------------------------
  # same plot as above but in 2 figures 
  # fig1 -- "cases", "deaths"
  # fig2 -- "yld",  "yll",  "dalys"
  
  for (j in 1:2) {
    
    # figure files
    tiff (paste0 ("figures/fig", j, ".png", sep=""), 
          units="in", width=6, height=9, res=900)
    
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
        facet_grid(scenario ~ simulation) +
        theme_bw (base_size = 10) +
        labs (
          x="Year of birth",
          y=y_axis[i]) + 
        scale_x_continuous(breaks=seq(2011, 2020, 3)) + 
        # theme_minimal () + 
        theme (axis.text.x = element_text(size=6))
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


# create table of country-specific cervical cancer burden
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
  
  # round off burden values (no decimal point)
  pre_vac <- pre_vac [, lapply(.SD, round, 0),
                      .SDcols = burden_columns,
                      by = .(country, simulation)]

  post_vac <- post_vac [, lapply(.SD, round, 0),
                      .SDcols = burden_columns,
                      by = .(country, simulation)]
  
  # sort by country and simulation scenario
  pre_vac  <- pre_vac  [order (country, simulation)]
  post_vac <- post_vac [order (country, simulation)]
  
  # add country name from iso3 country code
  pre_vac  [, Country := countrycode (pre_vac  [, country], origin = "iso3c", destination = "country.name")]
  post_vac [, Country := countrycode (post_vac [, country], origin = "iso3c", destination = "country.name")]
  
  # drop column iso3 country code 
  pre_vac  [, country := NULL]
  post_vac [, country := NULL]
  
  # set Country column as first column
  setcolorder (pre_vac,  "Country")
  setcolorder (post_vac, "Country")
  
  # add pre-vaccination / post-vaccination to burden column names
  setnames (pre_vac, 
            old = c("simulation", "cases", "deaths", "yld", "yll", "dalys"), 
            new = c("Scenario", "Cases (pre-vaccination)", "Deaths (pre-vaccination)", "YLDs (pre-vaccination)", "YLLs (pre-vaccination)", "DALYs (pre-vaccination)"))
  
  setnames (post_vac, 
            old = c("simulation", "cases", "deaths", "yld", "yll", "dalys"), 
            new = c("Scenario", "Cases (post-vaccination)", "Deaths (post-vaccination)", "YLDs (post-vaccination)", "YLLs (post-vaccination)", "DALYs (post-vaccination)"))
  
  # combine tables -- pre-vaccination (and) post-vaccination
  burden <- pre_vac [post_vac, on = .(Country = Country, Scenario = Scenario)]
  
  # save burden data table
  fwrite (burden, 
          "results/Table-Cervical_cancer_burden_HPV_16_18.csv",
          col.names = T, row.names = F)
  
  return ()  # return null

} # end of function -- create_table_country_burden











################################################################################




################################################################################

# ggplot (allburden [scenario == "post-vaccination" & simulation=="s2"], aes(x = birthcohort)) +
#   geom_col (aes(y = get("cases_p100"), col=age)) +
#   scale_colour_gradientn(colours=rev(rainbow(5))) +
#   theme_bw(base_size = 8) +
#   labs (
#     x="birth cohort year",
#     y=toplot)

################################################################################
# compute comparison metrics



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
# plot file

pdf ("plots/plots-impact.pdf")

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
# # for (i in 1:length(plotwhat)){
# for (i in 1:3){
#   toplot = plotwhat[i]
#   print (paste0 (i, toplot))
#   
#   p <- ggplot (vaccine_impact, 
#                  aes (x = simulation, y = get(toplot), fill=toplot)) + 
#     geom_bar (stat="identity") + 
#            labs (
#              x="Scenario",
#              y=toplot
#            # , title = "Vaccination impact per 1000 vaccinated girls"
#   ) + theme_minimal() + theme(legend.position="none") 
#   
#   plot_list[[i]] <- p
# }

plot_list <- lapply (1:length(plotwhat), function (i){
  toplot = plotwhat[i]
  
  p <- ggplot (vaccine_impact, 
               aes (x = simulation, y = get(toplot), fill=toplot)) + 
    geom_bar (stat="identity") + 
    labs (
      x="Scenario",
      y=y_axis[i]
      # , title = "Vaccination impact per 1000 vaccinated girls"
    ) + theme_minimal() + theme(legend.position="none") 
})

# q <- ggarrange(plot_list[[1]], 
#           plot_list[[2]], 
#           plot_list[[3]], 
#           plot_list[[4]], 
#           # plot_list[[5]], 
#           ncol = 3, nrow = 2)
q <- ggarrange(plotlist=plot_list, ncol = 3, nrow = 2)

annotate_figure(q, 
                top = text_grob("Lifetime health impact per 1000 vaccinated girls", color = "black", size = 14))

print (annotate_figure(q, 
                       top = text_grob("Lifetime health impact per 1000 vaccinated girls", color = "black", size = 14)))

dev.off ()




# cases proportion
# denominator <- burden_summary [simulation=="s1", .(scenario, total_cases)]
# burden_summary [scenario=="pre-vaccination",
#          cases_p := total_cases / denominator [scenario=="pre-vaccination", total_cases]]
# burden_summary [scenario=="post-vaccination",
#          cases_p := total_cases / denominator [scenario=="post-vaccination", total_cases]]

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

print (Sys.time ())  # end time

#-------------------------------------------------------------------------------
# save list of countries
# country_list <- foreach (i = unique(allburden$country)) %do% {
#     countrycode (i, origin = "iso3c", destination = "country.name") }
# country_list <- as.data.table (t (as.matrix (country_list)))
# fwrite (country_list, file="country_list.csv")
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# start of program
program_start_analyse ()  # load libraries, etc

# Combine burden estimates from different simulation scenarios 
allburden <- combine_burden_estimate ()

# plot cervical cancer burden (cases, deaths, yld, yll, dalys) pre- and post-vaccination
# plot for each country and at global level
# plot_cecx_burden_pre_post_vaccination (allburden)

# create table of country-specific cervical cancer burden
create_table_country_burden (allburden)

#-------------------------------------------------------------------------------
print (Sys.time ())  
# end of program
#-------------------------------------------------------------------------------
