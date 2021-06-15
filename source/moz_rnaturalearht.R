######## Mapping tutorial for Mozam

library(tidyverse)
theme_set(theme_void())
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sp)
library(raster)
library(spData)
library(tmap)
library(leaflet)
library(cowplot)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

### Basic data and plot

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)")) # with labels

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen") # adding colour

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") # population variable within world dataset 

#### Projection and extent
### The function coord_sf allows to deal with the coordinate system, which includes both projection and extent of the map

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

#### Zoom into an area

moz_dist$geometry # get the xy bounding box to project mozam 

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(30.21173, 40.84041), ylim = c(-26.86704, -10.47367), expand = FALSE)

## Scale bar and North arrow (package ggspatial)

ggplot(data = world) +
  
  geom_sf() +
  
  annotation_scale(location = "bl", width_hint = 0.5) +
  
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  coord_sf(xlim = c(30.21173, 40.84041), ylim = c(-26.86704, -10.47367))

## Country names and other names (geom_text and annotate)

country_names <- data.frame(name = c("Tanzania", 'Malawi', 'Zambia', 'Zimbabwe'),
                                         
                                         lon = c(-11, -13.282, -13.453, -19.394),
                                         
                                         lat = c(36.5, 33.806, 31.321, 31.5))


rivers110 <- ne_download(scale = 'medium', type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")

ocean <- ne_download(scale = 'medium', type = 'ocean', category = 'physical', returnclass = "sf")

rivers_cropped <- st_crop(st_as_sf(rivers110), xmin = 30.21173, xmax = 40.84041,
                          ymin = -26.86704, ymax = -10.47367)

ocean_crop <- st_crop(st_as_sf(ocean), xmin = 30.21173, xmax = 40.84041,
                      ymin = -26.86704, ymax = -10.47367)

lakes <- ne_download(scale = 'medium', type = 'lakes', category = 'physical', returnclass = "sf")

lake_crop <- st_crop(st_as_sf(lakes), xmin = 30.21173, xmax = 40.84041,
                      ymin = -18, ymax = -12.183333)

prov <- st_crop(st_as_sf(gadm36_MOZ_1_sp), xmin = 30.21173, xmax = 40.84041,
                 ymin = -26.86704, ymax = -10.47367)

roads <- read_sf('~/Mozambique_Stephen/mozambique_displacement_conflict/gisdata/moz_trs_roads_osm/moz_trs_roads_osm.shp')

main <- roads %>% 
  filter(ntlclass %in% c('primary', 'primary_link', 'secondary', 'secondary_link'))



moz_ipc <- st_as_sf(moz_ipc)
head(moz_ipc)
admin <- cbind(moz_ipc, st_coordinates(st_centroid(moz_ipc)))

#######

## ACLED conflict data by district 

acled_moz <- acled %>% 
  filter(year > 2017)

count_plot <- acled_moz %>%
  group_by(admin3, event_type) %>%
  summarise(count = n())

df_moz <- count_plot %>% 
  group_by(admin3, event_type) %>% 
  summarise(incidents = sum(count))

df_moz <- merge(df_moz, moz_dist, by.x = 'admin3', by.y = 'ADM2_PT', all.y = T)

############### FINAL GRAPHIC MAP ####################################################

breaks <- c(5, 20, 40)

ggplot() +
  
  geom_sf(data = admin, aes(X, Y, fill = area_phase_f), colour = scales::alpha("grey22",0)) +
  
  geom_sf(data = ocean, fill = 'lightblue') +
  
  geom_sf(data = world, aes(geometry = geometry), fill = NA) +
  
  geom_sf(data = main, col = 'white', size = 0.5) +
  
  geom_path(data = cabo_only, aes(long, lat, group = group), 
            colour = "#de2d26") +
  
  geom_point(data = df_moz, aes(size = incidents, geometry = geometry),
             stat = "sf_coordinates", shape = 19, col = '#252a77', alpha = 0.5) +
  
  scale_size_continuous(labels = breaks, breaks = breaks) +
  
  scale_fill_manual(values = c("#ead225","#f77b07", "#fee6ce"), guide="colorbar") +
  
  geom_text(data= country_names, aes(x = lat, y = lon, label = name),
            color = 'black', fontface = 'bold', check_overlap = FALSE, size = 3.8, family = 'Lato') +
  
  annotate(geom = "text", x = 38, y = -19.5, label = "Indian Ocean", 
           fontface = "italic", color = "grey22", size = 4.5, family = 'Lato') +
  
  annotate(geom = "text", x = 37, y = -13, label = "Main Roads", 
           fontface = "bold", color = "white", size = 2.9, family = 'Lato') +
  
  annotate(geom = "text", x = 39, y = -13.3, label = "Cabo Delgado", 
           fontface = "bold", color = "#de2d26", size = 2.5, family = 'Lato') +
  
  coord_sf(xlim = c(30.21173, 40.84041), ylim = c(-26.86704, -10.47367), expand = FALSE) +
  
  labs(title = 'Mozambique',
       x = '', y = '',
       caption = 'Data: ACLED (*Jan 2018-May 2021), Integrated Food Security Phase Classification
Graphic: Monique Bennett') +
  
  theme(text = element_text(family = 'Lato'),
        plot.title = element_text(face = 'bold', size = 17),
        plot.caption = element_text(size = 9.5),
        legend.position = c(.8, .2),
        legend.box.background = element_rect(color="white", size=0.5),
        legend.box.margin = margin(4, 4, 4, 4)) +
  
  guides(fill = guide_legend(title = paste(strwrap("Food insecurity phase (Apr-Sept 2021)", 25), collapse = "\n"))) +
  
  guides(size = guide_legend(title = 'Frequency of violence*')) 


ggsave('moz_ipc_acled_map.png', width = 6, height = 9, device = 'png')

#########

ggplot() +
  
  geom_sf(data = admin, aes(X, Y, fill = area_phase_f), colour = scales::alpha("grey22",0)) +
  
  geom_sf(data = ocean, fill = 'lightblue') +
  
  geom_sf(data = world, aes(geometry = geometry), fill = NA) +
  
  geom_sf(data = main, col = 'white', size = 0.5) +
  
  geom_path(data = cabo_only, aes(long, lat, group = group), 
            colour = "#de2d26") +
  
  geom_path(data = nampula, aes(long, lat, group = group), 
            colour = "#de2d26") +
  
  geom_path(data = nassa, aes(long, lat, group = group), 
            colour = "#de2d26") +
  
  geom_point(data = df_moz, aes(size = incidents, geometry = geometry),
             stat = "sf_coordinates", shape = 19, col = '#252a77', alpha = 0.5) +
  
  scale_size_continuous(labels = breaks, breaks = breaks) +
  
  scale_fill_manual(values = c("#ead225","#f77b07", "#fee6ce"), guide="colorbar") +
  
  geom_text(data= country_names, aes(x = lat, y = lon, label = name),
            color = 'black', fontface = 'bold', check_overlap = FALSE, size = 3.8, family = 'Lato') +
  
  annotate(geom = "text", x = 38, y = -19.5, label = "Indian Ocean", 
           fontface = "italic", color = "grey22", size = 4.5, family = 'Lato') +
  
  annotate(geom = "text", x = 37, y = -13, label = "Main Roads", 
           fontface = "bold", color = "white", size = 2.9, family = 'Lato') +
  
  annotate(geom = "text", x = 39, y = -13.3, label = "Cabo Delgado", 
           fontface = "bold", color = "#de2d26", size = 2.5, family = 'Lato') +
  
  annotate(geom = "text", x = 39, y = -15, label = "Nampula", 
           fontface = "bold", color = "#de2d26", size = 2.8, family = 'Lato') +
  
  annotate(geom = "text", x = 36, y = -12.5, label = "Niassa", 
           fontface = "bold", color = "#de2d26", size = 2.8, family = 'Lato') +
  
  coord_sf(xlim = c(30.21173, 40.84041), ylim = c(-26.86704, -10.47367), expand = FALSE) +
  
  labs(title = 'Mozambique',
       x = '', y = '',
       caption = 'Data: ACLED (*Jan 2018-May 2021), Integrated Food Security Phase Classification
Graphic: Monique Bennett') +
  
  theme(text = element_text(family = 'Lato'),
        plot.title = element_text(face = 'bold', size = 17),
        plot.caption = element_text(size = 9.5),
        legend.position = c(.8, .2),
        legend.box.background = element_rect(color="white", size=0.5),
        legend.box.margin = margin(4, 4, 4, 4)) +
  
  guides(fill = guide_legend(title = paste(strwrap("Food insecurity phase (Apr-Sept 2021)", 25), collapse = "\n"))) +
  
  guides(size = guide_legend(title = 'Frequency of violence*')) 


ggsave('moz_ipc_acled_map_north.png', width = 6, height = 9, device = 'png')

