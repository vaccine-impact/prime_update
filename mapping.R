# create maps

# map tutorial
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

library ("ggplot2")
library ("sf")
library ("rnaturalearth")
library ("rnaturalearthdata")
library (rgeos)
library (data.table)



# Title, subtitle, and axis labels (ggtitle, xlab, ylab)
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

# Map color (geom_sf)
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Saving the map with ggsave
# ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")

# ------------------------------------------------------------------------------
theme_set(theme_bw())

d <- fread (file = "results/Table-Vaccine_impact_s5_iso3.csv")
setDT (world)
dt <- world [d, on = .(iso_a3 = country)]

ggplot(data = dt) +
  geom_sf (aes(fill = deaths_averted_perVG, geometry = geometry)) + 
  scale_fill_viridis_c(option = "C") + 
  ggtitle ("Deaths averted per 1000 vaccinated girls") + 
  theme(legend.title = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank())

ggsave ("map_web.png", width = 6, height = 6, dpi = "screen")

  # scale_colour_continuous ()
  # scale_fill_gradientn (colours = terrain.colors(80))
  # scale_color_distiller(palette = "RdPu")
  # scale_fill_gradient2 ()
  # scale_fill_gradientn (colours = rev(rainbow(5)))
  # scale_fill_viridis_c(option = "E")





