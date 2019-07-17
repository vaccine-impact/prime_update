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


create_map <- function () {
  
  # map tutorial
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  world <- ne_countries (scale = "medium", returnclass = "sf")
  
  # ------------------------------------------------------------------------------
  theme_set(theme_bw())
  
  d <- fread (file = "results/Table-Vaccine_impact_s5_iso3.csv")
  setDT (world)
  dt <- world [d, on = .(iso_a3 = country)]
  
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
  
  tiff ("figures/Figure-Burden_averted_per1000FVG.png",
        units="in", width=6, height=7.5, res=900)
  
  print (q)
  
  dev.off ()
  
  return ()
}

#-------------------------------------------------------------------------------
create_map ()







