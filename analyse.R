# File: analyse.R
# Compare burden estimates and vaccination impact from the different scenarios.

################################################################################
# 1) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2012"
# 2) unwpp_mortality = TRUE,  disability.weights = "gbd_2001", canc.inc = "2012"
# 3) unwpp_mortality = FALSE, disability.weights = "gbd_2017", canc.inc = "2012"
# 4) unwpp_mortality = FALSE, disability.weights = "gbd_2001", canc.inc = "2018"
# 5) unwpp_mortality = TRUE,  disability.weights = "gbd_2017", canc.inc = "2018"
################################################################################

print (Sys.time ())  # start time


# load required packages
library (data.table)    # data table
library (ggplot2)       # graphics
library (tictoc)
library (countrycode)
library (ggforce)
library (ggpubr)

# remove all objects from workspace
remove (list = objects() )

# plot file
pdf ("plots/plots-compare.pdf")

# simulation scenarios
simulations = c("s1", "s2", "s3", "s4", "s5")

################################################################################
# Combine burden estimates from different simulations

# burden estimates for different scenarios
allburden <- NULL
for (i in 1:length(simulations)) {
  burdenfile <- paste0 ("output/", simulations[i], "_results.csv")
  burden <- fread (burdenfile, header = "auto", stringsAsFactors = F)

  # extract birth cohorts between 1991 & 2021
  # burden <- burden [birthcohort >= 1991 & birthcohort <= 2021]

  burden [, simulation := simulations[i]]
  if (is.null(allburden)) {
    allburden <- burden
  } else {
    allburden <- rbind (allburden, burden)
  }
}
setDT (allburden)

# add calendar year, cases, deaths, yll, yld, dalys
allburden [, year   := birthcohort + age]
allburden [, cases  := cohort_size * inc.cecx]
allburden [, deaths := cohort_size * mort.cecx]
allburden [, yld    := cohort_size * disability]
allburden [, yll    := cohort_size * lifey]
allburden [, dalys  := yll + yld]

allburden [, cases_p100  := cases  / cohort_size * 100000]
allburden [, deaths_p100 := deaths / cohort_size * 100000]
allburden [, yld_p100    := yld    / cohort_size * 100000]
allburden [, yll_p100    := yll    / cohort_size * 100000]
allburden [, dalys_p100  := dalys  / cohort_size * 100000]

# NA values result due to division by 0 for UNWPP simulations, 
# since cohort size for ages 0 to 7 are 0
allburden [is.na(allburden)] <- 0

# vaccined administered to 9 year old girls
allburden [(scenario=="post-vaccination" & age==9), vaccines := cohort_size * vaccinated, with=T]

################################################################################
# Plots

plotwhat = c("cohort_size", "cases", "deaths", "yld", "yll", "dalys")
# ,
#              "cases_p100", "deaths_p100", "yld_p100", "yll_p100", "dalys_p100")

y_axis <- c("Cohort size",
            "Cases",
            "Deaths",
            "YLDs",
            "YLLs", 
            "DALYs")

counter <- 0
for (countries in unique (allburden$country)) {
  
  if (counter <177) {  # plot subset of countries
    counter <- counter + 1
    
    tic ()
    print (countries)
    country_burden <- allburden [country == countries]
    
    for (i in 1:length(plotwhat)){
      toplot = plotwhat[i]
      # print (ggplot (country_burden, aes(x = birthcohort)) +
      #          geom_col (aes(y = get(toplot), col=age)) +
      #          scale_colour_gradientn(colours=rev(rainbow(5))) +
      #          facet_grid(scenario ~ simulation) +
      #          theme_bw(base_size = 8) +
      #          labs (
      #            x="birth cohort year",
      #            y=toplot,
      #            title = countrycode (countries, 'iso3c', 'country.name'))
      #            # title = countries)
      # )
      
      print (ggplot (country_burden, 
                     aes (x = birthcohort, y = get(toplot), fill=age)) +
             geom_bar (stat="identity") + 
             scale_fill_gradientn(colours=rev(rainbow(5))) + 
             facet_grid(scenario ~ simulation) +
             theme_bw(base_size = 8) +
             labs (
               x="Year of birth",
               y=y_axis[i],
               title = countrycode (countries, 'iso3c', 'country.name')) + 
             theme_minimal()
             # title = countries)
      )
    }
    
    toc ()
  }
}

dev.off ()  # close plot file

################################################################################
# Similar burden comparison as in the above plots but one plot at the global level


gburden <- allburden 
gburden [, country := NULL]
global_burden <- gburden [, lapply (.SD, sum), by=.(age, scenario, type, simulation, birthcohort)]
# dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]

pdf ("plots/global-compare.pdf")

for (i in 1:length(plotwhat)) {
  toplot = plotwhat[i]
  
  print (ggplot (global_burden, 
                 aes (x = birthcohort, y = get(toplot), fill=age)) +
           geom_bar (stat="identity") + 
           scale_fill_gradientn(colours=rev(rainbow(5))) + 
           facet_grid(scenario ~ simulation) +
           theme_bw(base_size = 8) +
           labs (
             x="Year of birth",
             y=y_axis[i]) + 
           scale_x_continuous(breaks=seq(2011, 2020, 3)) + 
             # title = countrycode (countries, 'iso3c', 'country.name')) + 
           theme_minimal() 
         # title = countries) 
  )
}

dev.off ()

# ------------------------------------------------------------------------------
#### same plot as above but in 1 page
plotwhat = c("cohort_size", "cases", "deaths", "yld", "yll", "dalys")

plot_list <- lapply (2:length(plotwhat), function (i) {
  toplot = plotwhat[i]
  
  p <- ggplot (global_burden, 
               aes (x = birthcohort, y = get(toplot), fill=age)) +
    geom_bar (stat="identity") + 
    scale_fill_gradientn(colours=rev(rainbow(5))) + 
    facet_grid(scenario ~ simulation) +
    theme_bw(base_size = 8) +
    labs (
      x="Year of birth",
      y=y_axis[i]) + 
    scale_x_continuous(breaks=seq(2011, 2020, 3)) + 
    theme_minimal() + 
    theme (axis.text.x = element_text(size=6))
})


q <- ggarrange (plotlist=plot_list, ncol = 2, nrow = 3)

print (annotate_figure(q,
                top = text_grob("Lifetime health impact pre- and post-vaccination", color = "black", size = 14)))

# ------------------------------------------------------------------------------











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