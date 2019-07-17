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


create_map <- function (vaccine_impact) {
  
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
    scale_fill_viridis_c(option = "C") + 
    ggtitle ("Cases averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
  
  ggsave ("maps/cases_averted_per1000FVG.png", width = 6, height = 6, dpi = "screen")
  
  # map of deaths averted per 1000 vaccinated girls
  deaths_a <- ggplot(data = dt) +
    geom_sf (aes(fill = deaths_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "C") + 
    ggtitle ("Deaths averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
  
  ggsave ("maps/deaths_averted_per1000FVG.png", width = 6, height = 6, dpi = "screen")
  
  # map of YLDs averted per 1000 vaccinated girls
  ylds_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yld_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "C") + 
    ggtitle ("YLDs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
  
  ggsave ("maps/ylds_averted_per1000FVG.png", width = 6, height = 6, dpi = "screen")
  
  # map of YLLs averted per 1000 vaccinated girls
  ylls_a <- ggplot(data = dt) +
    geom_sf (aes(fill = yll_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "C") + 
    ggtitle ("YLLs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
  
  ggsave ("maps/ylls_averted_per1000FVG.png", width = 6, height = 6, dpi = "screen")
  
  # map of DALYs averted per 1000 vaccinated girls
  dalys_a <- ggplot(data = dt) +
    geom_sf (aes(fill = dalys_averted_perVG, geometry = geometry)) + 
    scale_fill_viridis_c(option = "C") + 
    ggtitle ("DALYs averted per 1000 vaccinated girls") + 
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
  
  ggsave ("maps/dalys_averted_per1000FVG.png", width = 6, height = 6, dpi = "screen")
  
  
  # arrange plots of (cases, deaths and DALYs) averted per 1000 vaccinated girls
  # in a single page
  
  plot_list <- list (cases_a, deaths_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3)
  
  tiff ("figures/Figure-Burden_averted_per1000FVG_cases_deaths_dalys.png",
        units="in", width=6, height=7.5, res=900)
  
  print (q)
  dev.off ()
  
  # arrange plots of (YLDs, YLLs, DALYs) averted per 1000 vaccinated girls
  # in a single page
  
  plot_list <- list (ylds_a, ylls_a, dalys_a)
  q <- ggarrange (plotlist=plot_list, ncol = 1, nrow = 3)
  
  tiff ("figures/Figure-Burden_averted_per1000FVG_ylds_ylls_dalys.png",
        units="in", width=6, height=7.5, res=900)
  
  print (q)
  dev.off ()
  
  return ()
}


# generate summary statistics
compute_summary_stats <- function (vaccine_impact) {
  
  burden_averted <- colnames (vaccine_impact)[3:7]
  
  for (i in 1:5) {
    
    # print summary statistics
    print (burden_averted[i])
    print (summary (vaccine_impact [, get(burden_averted [i])]))
    print (quantile (vaccine_impact [, get(burden_averted [i])], c(0.025, 0.975)))
    
  }
}


#-------------------------------------------------------------------------------
# read vaccine impact table for updated PRIME
vaccine_impact <- fread (file = "results/Table-Vaccine_impact_s5_iso3.csv")

# create map
create_map (vaccine_impact)

# generate summary statistics
compute_summary_stats (vaccine_impact)









