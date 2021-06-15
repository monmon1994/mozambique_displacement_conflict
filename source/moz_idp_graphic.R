library(tidyverse)
library(readr)
library(readxl)
library(rio)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(extrafont)
library(lubridate)
library(wesanderson)
library(dplyr)
loadfonts()
##### Displacement Data from humdata

moz19 <- read_excel('data/dtm-moz-r19.xlsx')

moz19 <- moz19[-1, ]

df <- data.frame(moz19$`1.1.a.1 Date of Current Survey`, moz19$`1.1.d.1 Site Name`, moz19$Longitude, 
                 moz19$Latitude,  moz19$`2.1.b.1 Total number of IDP individuals`)

colnames(df) <- c("date", 'site_name', 'long', 'lat', 'IDPs')

df$IDPs <- as.numeric(df$IDPs)

df$long <- as.numeric(df$long)
df$lat <- as.numeric(df$lat)

sum(df$IDPs)

######## Other IDP data

df <- read_excel('data/cabo_idps.xlsx')


#### Displacement line graph

ggplot(df) +
  
  geom_line(aes(x = survey_round, y = idps, colour = district, group = district), size = 1.4) +
  
  geom_point(aes(x = survey_round, y = idps, colour = district, group = district), size = 2.5) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::comma_format(accuracy = 1),
                     position = 'right', expand = c(0, 0)) +
  
  labs(title = 'Displacement in Cabo Delgado districts',
       x = 'IDM Survey Round', y = '') +
  
  scale_colour_manual(values = cols) +
  
  theme_minimal(base_family = 'Lato') +

  theme(legend.position = 'bottom')

########## Stacked area plot

cols <- wes_palette("Darjeeling1", 8, type = 'continuous')

line <- data.frame(df$survey_round, df$total)

line <- na.omit(line)

ggplot(df) +
  
  geom_area(aes(x = survey_round, y = idps, fill = district, group = district), alpha = 0.8) +
  
  scale_fill_manual(values = cols) +
  
  expand_limits(y = c(0, 650000)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::comma_format(accuracy = 1),
                     position = 'right', expand = c(0, 0)) +
  
  labs(title = 'Total population of IDPs present across districts in Cabo Delgado', 
       x = 'DTM Survey Round', y = '',
       caption = 'Source: Displacement Tracking Matrix (DTM) and the International Organization for Migration, April 2021
Graphic: Monique Bennett') +
  
  theme_minimal(base_family = 'Lato', base_size = 12) +
  
  theme(legend.position = 'bottom',
        plot.caption = element_text(size = 8)) +
  
  guides(fill = guide_legend(title = 'District'))

ggsave('idps_moz.svg', device = 'svg')

ggsave('idps_moz.png', device = 'png')

#### STREAM plot for fun
library(ggstream)

ggplot(df) +
  
  geom_stream(aes(x = survey_round, y = idps, fill = district)) +
  
  scale_fill_manual(values = cols) +
  
  theme_minimal()

############################ CONFLICT DATA

acled <- read_excel('data/acled_moz.xlsx')

cabo_acled <- read_excel('data/mozambique_org_viol_May28.xlsx')

## ACLED conflict data by district 

cabo <- cabo_acled %>% 
  filter(year > 2018)

count_plot <- cabo %>%
  group_by(event_date, admin1, admin3, event_type, sub_event_type) %>%
  summarise(count = n())

df_count <- count_plot %>% 
  filter(admin1 == 'Cabo Delgado') %>% 
  group_by(month = floor_date(event_date, 'month'),
           admin1, admin3, event_type, sub_event_type) %>% 
  summarise(incidents = sum(count))

df_count$month <- ymd(df_count$month)

######### looking at actors and sub_event_type

unique(cabo_acled$actor1)

unique(cabo_acled$sub_event_type)

##### Stacked graph

ggplot(df_count) +
  
  geom_area(aes(x = month, y = incidents, fill = sub_event_type)) +
  
  scale_fill_manual(values = cols) +
  
  scale_x_date(date_labels = "%b-%y") +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::comma_format(accuracy = 1),
                     position = 'right', expand = c(0, 0)) +
  
  labs(title = 'Incidents of Violence and Insecurity in Cabo Delgado', 
       x = '', y = '') +
  
  theme_minimal(base_family = 'Lato', base_size = 12) +
  
  theme(legend.position = 'bottom') +
  
  guides(fill = guide_legend(title = ''))

ggsave('cabo_violence.svg', device = 'svg')

#### Bar graph

ggplot(df_count) +
  
  geom_col(aes(x = month, y = incidents, fill = sub_event_type)) +
  
  scale_fill_manual(values = cols) +
  
  scale_x_date(date_labels = "%b-%y", date_breaks = '2 months') +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::comma_format(accuracy = 1),
                     position = 'left', expand = c(0, 0)) +
  
  labs(title = 'Violence and Insecurity in Cabo Delgado, Mozambique', 
       x = '', y = '', subtitle = 'Number of incidents recorded between Jan 2018 and May 2021',
       caption = 'Data: Armed Conflict Location & Event Data Project 
Graphic: Monique Bennett') +
  
  theme_minimal(base_family = 'Lato', base_size = 12) +
  
  theme(legend.position = 'bottom',
        plot.caption = element_text(size = 9),
        plot.title = element_text(face = 'bold')) +
  
  guides(fill = guide_legend(title = ''))

ggsave('bar_moz_acled.png', device = 'png', width = 9, height = 7)


