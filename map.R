# File: map.R
# Create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination.

# load libraries
library (ggplot2)
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)
library (rgeos)
library (data.table)
library (ggforce)
library (ggpubr)
library (stats)

rm (list = ls ())

# ------------------------------------------------------------------------------
# change column names for burden averted
# ------------------------------------------------------------------------------
change_colnames <- function (vaccine_impact) {
  
  # update column names for burden averted
  setnames (vaccine_impact, 
            old = c("Country code (ISO 3)", 
                    "WHO region", 
                    "Cases averted per 1000 vaccinated girls", 
                    "Deaths averted per 1000 vaccinated girls", 
                    "YLDs averted per 1000 vaccinated girls", 
                    "YLLs averted per 1000 vaccinated girls", 
                    "DALYs averted per 1000 vaccinated girls"), 
            new = c("country", 
                    "who_region", 
                    "cases_averted_perVG", 
                    "deaths_averted_perVG", 
                    "yld_averted_perVG", 
                    "yll_averted_perVG", 
                    "dalys_averted_perVG"))
  
  return (vaccine_impact)
  
} # end of function -- change_colnames
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# create maps of cervical cancer burden (caused by HPV 16/18) averted by HPV vaccination
# ------------------------------------------------------------------------------
create_map <- function (vaccine_impact, scenario) {
  
  # map tutorial
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  world <- ne_countries (scale = "medium", returnclass = "sf")
  setDT (world)
  
  # ------------------------------------------------------------------------------
  theme_set(theme_bw())
  
  # combine tables to add geometry
  dt <- world [vaccine_impact, on = .(iso_a3 = country)]
  
  # map of cases averted per 1000 vaccinated girls
  cases_a <- ggplot(data = dt) +
    geom_sf (aes(fill = cases_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    ggtitle ("Cases averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave (paste0 ("maps/", scenario, "_cases_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of deaths averted per 1000 vaccinated girls
  deaths_a <- ggplot(data = dt) +
    geom_sf (aes(fill = deaths_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    ggtitle ("Deaths averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave (paste0 ("maps/", scenario, "_deaths_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of YLDs averted per 1000 vaccinated girls
  ylds_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yld_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("YLDs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave (paste0 ("maps/", scenario, "_ylds_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of YLLs averted per 1000 vaccinated girls
  ylls_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yll_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("YLLs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave (paste0 ("maps/", scenario, "_ylls_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of DALYs averted per 1000 vaccinated girls
  dalys_a <- ggplot(data = dt) +
    geom_sf (aes(fill = dalys_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("DALYs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave (paste0 ("maps/", scenario, "_dalys_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  
  # arrange plots of (cases, deaths and DALYs) averted per 1000 vaccinated girls
  # in a single page
  
  plot_list <- list (cases_a, deaths_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3, 
                  labels = paste0 ("(scenario ", scenario, ")"), hjust = -4)
  
  # save a copy of s5 map (cases, deaths, dalys) without mentioning s5 in the map
  if (scenario == "s5") {
    s5_map <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3)
    ggsave (filename = "figures/Figure-Map_updated_burden_averted_per1000FVG_cases_deaths_dalys.png", 
            plot = s5_map, 
            width = 6, height = 7.5, units="in", dpi = 300)
  }
  
  # print plot of maps
  print (q)
  
  # arrange plots of (YLDs, YLLs, DALYs) averted per 1000 vaccinated girls
  # in a single page
  plot_list <- list (ylds_a, ylls_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3, 
                  labels = paste0 ("(scenario ", scenario, ")"), hjust = -4)
  
  # print plot of maps
  print (q)
  
  return ()
  
} # end of function -- create_map
# ------------------------------------------------------------------------------


# generate summary statistics
compute_summary_stats <- function (vaccine_impact, scenario) {
  
  # print scenario
  print (paste0 ("Scenario = ", scenario))
  
  burden_averted <- colnames (vaccine_impact)[5:9]
  
  for (i in 1:5) {
    # print summary statistics
    print (burden_averted[i])
    print (summary (vaccine_impact [, get(burden_averted [i])]))
    print (quantile (vaccine_impact [, get(burden_averted [i])], c(0.025, 0.975)))
  }
  
} # end of function -- compute_summary_stats
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# read vaccine impact table
vaccine_impact <- fread (file = "appendix/Table-Vaccine_impact.csv")

# change column names for burden averted
vaccine_impact <- change_colnames (vaccine_impact)

pdf ("appendix/Figure_Global_maps_vaccine_impact_scenarios.pdf")

# create map
for (i in c("s1", "s2", "s3", "s4", "s5")) {
  # for (i in c("s5")) {
  print (i)  # run status
  
  # extract rows for a specific scenario
  vaccine_impact_scenario <- vaccine_impact [Scenario == i]
  
  # create map for a specific scenario
  create_map (vaccine_impact_scenario, i)
  
  # generate summary statistics
  compute_summary_stats (vaccine_impact_scenario, i)
}

dev.off ()












