######### Mapping Mozambique ############

library(maptools)
library(readxl)
library(raster)
library(tidyverse)
library(rgdal)
library(ggmap)
library(scales)
library(extrafont)
library(ggthemes)
library(ggrepel)
library(sp)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(maps)
library(broom)
library(viridis)
loadfonts(device = "win", quiet = T)

### Load data

######## IDP data

idps <- read_excel('data/cabo_idps.xlsx')

acled <- read_excel('data/acled_moz.xlsx')

cabo_acled <- read_excel('data/mozambique_org_viol_May28.xlsx')

######### Food insecurity data

ipc <- read_excel('data/food_insecurity_ipc.xlsx')

###### Shape files for MOZ

load("data/moz_sf_objects.RData")

moz_dist$dist0 <- iconv(moz_dist$ADM2_PT, from = "UTF-8", 
                        to = "ASCII//TRANSLIT")

moz_north <- moz_dist %>% 
  filter(ADM1_PT %in% c('Cabo Delgado', 'Nampula', 'Niassa', 'Tete', 'Zambezia', 'Sofala', 'Manica',
                        'Inhambane', 'Gaza', 'Maputo'))


moz_df <- merge(df, moz_north, by.x = 'district', by.y = 'ADM2_PT', all.y = T)

plot(moz_dist)

################ Maps ###########

moz_df %>% 
  filter(survey_round == 12) %>% 
  
ggplot() +
  
  geom_sf(data = moz_north, aes(geometry = geometry)) +
  
  geom_sf(aes(fill = idps, geometry = geometry), show.legend = T, stat = 'sf') +
  
  scale_fill_gradientn(colours = cols) +
  
  labs(title = "IDPs in Cabo Delgado", fill = "IDPs") +
  
  theme(legend.position = "right")


######################### Cabo ##################################################


cabo_mp <- moz[moz@data$NAME_1 == "Cabo Delgado", ]

cabo_mp$NAME_2 <- iconv(cabo_mp$NAME_2, 
                        from = "UTF-8", to = "ASCII//TRANSLIT")

moz_north <- moz_dist %>% 
  filter(ADM1_PT %in% c('Cabo Delgado', 'Namula', 'Niassa', 'Tete'))

cabo_mp@data$id <- rownames(cabo_mp@data)

cabo_dis <- fortify(cabo_mp)

cabo.df <- plyr::join(cabo_dis, cabo_mp@data, by = 'id')

cabo_idp <- merge(cabo.df, df, by.x = 'NAME_2', by.y = 'district', all.x=T)


############################################################# 

moz_idp <- merge(idps, moz_north, by.x = 'district', by.y = 'ADM2_PT', all.y = T)

labels <- moz_idp %>% 
  select(district, province, geometry) %>% 
  filter(province == "Cabo Delgado") %>% 
  distinct()

ggplot() +
  
  geom_sf(data = moz_idp, aes(geometry = geometry, fill = idps), show.legend = T) +
  
  scale_fill_gradientn(colours = color_Blues) +
  
  labs(title = "Cabo Delgado IDPs", fill = "Number of IDPs") +
  
  ggrepel::geom_label_repel(data = labels,
    aes(label = district, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0) +
  
  theme_void() +
  
  theme(legend.position = "right")

############## ACLED and Food Security #########

moz_acled <- merge(df_count, moz_dist, by.x = 'admin3', by.y = 'ADM2_PT', all.y = T)

moz_ipc <- merge(ipc, moz_dist, by.x = 'district', by.y = 'ADM2_PT', all.y = T)

moz_ipc$area_phase[is.na(moz_ipc$area_phase)] <- 1

moz_ipc$area_phase_f <- factor(moz_ipc$area_phase, levels = c(2, 3, 1),
                                labels = c('Stressed', 'Crisis', 'Area not analysed'))

moz_prov <- getData("GADM", country="MOZ", level=1)
cabo_only <- moz_prov[moz_prov@data$NAME_1 == 'Cabo Delgado',] # CABO
cabo_only <- fortify(cabo_only)

nassa <- moz_prov[moz_prov@data$NAME_1 == 'Nassa',] # NIASSA
nassa <- fortify(nassa)

nampula <- moz_prov[moz_prov@data$NAME_1 == 'Nampula',] # NAMPULA
nampula <- fortify(nampula)

ggplot() +
  
  geom_sf(data = moz_ipc, aes(geometry = geometry, fill = area_phase_f), show.legend = T) +
  
  geom_point(data = moz_acled, aes(size = incidents, geometry = geometry),
    stat = "sf_coordinates", shape = 1, colour = 'red') +
  
  scale_fill_manual(values = c("#FFFF00", "#FC4E07")) +
  
  labs(title = "", fill= "IPC Food Insecurity") +
  
  theme_minimal() +
  
  theme(legend.position = "right") 



