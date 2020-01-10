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

# rm (list = ls ())

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
create_map <- function (vaccine_impact, 
                        scenario, 
                        vaccine, 
                        vaccination_age) {
  
  # set vaccine type
  vaccine_type <- switch (vaccine, 
                          "4vHPV" = "bivalent/quadrivalent", 
                          "9vHPV" = "nonavalent")
  
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
    # labs (title = "Cases averted per 1000 vaccinated girls", 
    #       subtitle = paste0 ("(vaccination age = ", vaccination_age,  
    #                          " years / vaccine = ", vaccine_type, " vaccine)") ) + 
    theme (legend.title = element_blank()) + 
    theme (axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme (axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme (plot.title = element_text(size = 10))
  
  ggsave (paste0 ("maps/", scenario, "_cases_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of deaths averted per 1000 vaccinated girls
  deaths_a <- ggplot(data = dt) +
    geom_sf (aes(fill = deaths_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    ggtitle ("Deaths averted per 1000 vaccinated girls") + 
    # labs (title = "Deaths averted per 1000 vaccinated girls", 
    #       subtitle = paste0 ("(vaccination age = ", vaccination_age,  
    #                          " years / vaccine = ", vaccine_type, " vaccine)") ) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme (plot.title = element_text(size = 10))
  
  ggsave (paste0 ("maps/", scenario, "_deaths_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of YLDs averted per 1000 vaccinated girls
  ylds_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yld_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("YLDs averted per 1000 vaccinated girls") + 
    # labs (title = "YLDs averted per 1000 vaccinated girls", 
    #       subtitle = paste0 ("(vaccination age = ", vaccination_age,  
    #                          " years / vaccine = ", vaccine_type, " vaccine)") ) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme (plot.title = element_text(size = 10))
  
  ggsave (paste0 ("maps/", scenario, "_ylds_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of YLLs averted per 1000 vaccinated girls
  ylls_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yll_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("YLLs averted per 1000 vaccinated girls") + 
    # labs (title = "YLLs averted per 1000 vaccinated girls", 
    #       subtitle = paste0 ("(vaccination age = ", vaccination_age,  
    #                          " years / vaccine = ", vaccine_type, " vaccine)") ) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme (plot.title = element_text(size = 10))
  
  ggsave (paste0 ("maps/", scenario, "_ylls_averted_per1000FVG.png"), 
          width = 6, height = 6, dpi = "screen")
  
  # map of DALYs averted per 1000 vaccinated girls
  dalys_a <- ggplot(data = dt) +
    geom_sf (aes(fill = dalys_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) + 
    ggtitle ("DALYs averted per 1000 vaccinated girls") + 
    # labs (title = "DALYs averted per 1000 vaccinated girls", 
    #       subtitle = paste0 ("(vaccination age = ", vaccination_age,  
    #                          " years / vaccine = ", vaccine_type, " vaccine)") ) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme (plot.title = element_text(size = 10))
  
  ggsave (paste0 ("maps/", scenario, "_dalys_averted_per1000FVG.png"), 
          width = 6, height = 7, dpi = "screen")
  
  
  # arrange plots of (cases, deaths and DALYs) averted per 1000 vaccinated girls
  # in a single page
  
  label_name <- paste0 ("scenario ", scenario, 
                        " / vaccination age = ", vaccination_age, 
                        " years / ", vaccine_type, " vaccine") 
  print (label_name)
  
  plot_list <- list (cases_a, deaths_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3, 
                  labels = label_name, 
                  font.label = list (size = 6, color = "black", face =
                                      "plain", family = NULL), 
                  hjust = -1.6)
  
  # save a copy of s5 map (cases, deaths, dalys) without mentioning s5 in the map
  if (scenario == "s5") {
    
    label_name_s5 <- paste0 ("Lifetime health impact of ", vaccine_type, 
                          " HPV vaccination of ", vaccination_age, 
                          "-year-old girls")
    
    s5_map <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3, 
                         labels = label_name_s5, 
                         font.label = list (size = 11, color = "black", 
                                            face = "plain", family = NULL), 
                         hjust = -0.05
                         )
    
    ggsave (filename = paste0 ("figures/Figure-Map_s5_burden_averted_per1000FVG_cases_deaths_dalys_age", 
                               vaccination_age, "_", vaccine, ".png"),
            plot = s5_map, 
            width = 6, height = 8, units="in", dpi = 300)
    
    # --------------------------------------------------------------------------
    # save figure in eps format for paper 
    ggsave (filename = paste0 ("figures/Figure-Map_s5_burden_averted_per1000FVG_cases_deaths_dalys_age", 
                               vaccination_age, "_", vaccine, ".eps"),
            plot = s5_map, 
            width = 6, height = 8.0, units="in")
    # --------------------------------------------------------------------------
  }
  
  # print plot of maps
  print (q)
  
  # arrange plots of (YLDs, YLLs, DALYs) averted per 1000 vaccinated girls
  # in a single page
  plot_list <- list (ylds_a, ylls_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3, 
                  labels = label_name, 
                  font.label = list (size = 6, color = "black", 
                                     face = "plain", family = NULL), 
                  hjust = -1.6)
  
  # print plot of maps
  print (q)
  
  return ()
  
} # end of function -- create_map
# ------------------------------------------------------------------------------


# generate summary statistics
compute_summary_stats <- function (vaccine_impact, 
                                   scenario, 
                                   vaccine, 
                                   vaccination_age) {
  
  # set vaccine type
  vaccine_type <- switch (vaccine, 
                          "4vHPV" = "bivalent/quadrivalent", 
                          "9vHPV" = "nonavalent")
  
  # print scenario
  cat (paste0 ("\n Scenario = ", scenario, 
                 " / vaccination age = ", vaccination_age, 
                 " /  vaccine = ", vaccine_type) )
  
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
# start of program
print (Sys.time ())
# ------------------------------------------------------------------------------

# loop through vaccination ages
for (vaccination_age in vaccination_ages) {
  
  # loop through vaccines
  for (vaccine in vaccines) {
    
    # read vaccine impact table
    vaccine_impact <- fread (file = paste0 ("appendix/Table-Vaccine_impact_age", 
                                            vaccination_age, "_", vaccine, ".csv"))
                                            
    # change column names for burden averted
    vaccine_impact <- change_colnames (vaccine_impact)
    
    pdf (paste0 ("appendix/Figure_Global_maps_vaccine_impact_scenarios_age", 
                 vaccination_age, "_", vaccine, ".pdf"))
    
    # save Results (numbers) for the paper
    sink (file = paste0 ("paper_results/results_age", 
                  vaccination_age, "_", vaccine, ".txt"),  
         append = F, 
         split  = TRUE)
    
    # create map
    for (i in c("s1", "s2", "s3", "s4", "s5")) {   

      # extract rows for a specific scenario
      vaccine_impact_scenario <- vaccine_impact [Scenario == i]
      
      # create map for a specific scenario
      create_map (vaccine_impact_scenario, 
                  i, 
                  vaccine         = vaccine, 
                  vaccination_age = vaccination_age)
      
      # generate summary statistics
      compute_summary_stats (vaccine_impact_scenario, 
                             i, 
                             vaccine         = vaccine, 
                             vaccination_age = vaccination_age)
    }
    
    sink ()
    
    dev.off ()
    
  }
}

# ------------------------------------------------------------------------------
print (Sys.time ())
# end of program
# ------------------------------------------------------------------------------













