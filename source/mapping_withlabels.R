library(tidyverse)
library(ggrepel)
library(ggplot2)
library(extrafont)
library(ggthemes)
library(sp)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(maptools)
library(maps)
library(readxl)

AfricaLowCovid <- read_excel("AfricaLowCovid.xlsx")

colnames(AfricaLowCovid) <- c("country", "population_2020", "cumulative_tests", "confirmed_cases", "deaths", "recoveries",
                              "recovery_rate", "case_fatality_rate", "GDP_per_capita", "median_age", "population_above_65", "physicians_per_1000","people_using_basic_sanitation_services",
                              "people_using_basic_drinking_water_services" ,"pop_density_sqkm","HDI_2018" ,"GHSI_2019" ,"tests_conducted_per_new_case",
                              "percent_of_population_tested", "trust_in_science_health_professionals", "GFSI", "merchandise_%_of_GDP",
                              "Iso3" ,"Internet_penetration", "Corruption_Perception_Index", "Facebook_Subscribers", "FB_percent_Pop")


#Global Food Security Index

world$Iso3 <- countrycode::countrycode(world$name_long, "country.name", "iso3c")

afr_GFSI <-world %>%
    filter(continent == "Africa") %>%
    left_join(AfricaLowCovid, by = "Iso3") 

ggplot(afr_GFSI) +
    geom_sf(aes(fill = GFSI), color = "black") +
    scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = -1 , aesthetics = "fill",
                         breaks = c(60, 38), labels = c("Best", "Poor")) +
    labs(title = "Global Food Security Index",
         subtitle = "The score out of 100 indicates the level of food security performance. 
       A score closer to 0 is poor and towards 100 better. Grey countries indicate no data.") +
    ggrepel::geom_label_repel(
        aes(label = country, geometry = geom),
        stat = "sf_coordinates",
        min.segment.length = 0) +
    theme_void() +
    theme(text = element_text(family = "Times New Romans", size = 13),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = c(0.3, 0.3),
          plot.subtitle = element_text(hjust = 0.5)) 

ggsave("Figure_14.10_labels.png", plot = last_plot(), dpi = 800)