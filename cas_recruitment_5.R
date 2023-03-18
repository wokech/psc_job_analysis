# An analysis of the CAS recruitment in Kenya (Part 5)

# Notes:
# 1) Gradients - https://digitalsynopsis.com/design/beautiful-color-ui-gradients-backgrounds/
# 2) https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

### Libraries

library(tidyverse)
library(tidyr)
library(janitor)
library(tidyr)
library(readxl)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)
library(treemapify)
# install.packages("ggrepel")
library(ggrepel)

# RATIOS

# a) What is the ratio of males to females that applied per county?

all_cas_gender_county_df <- data.frame(tabyl(all_cas, Gender, County))

all_cas_gender_county_df_long <- all_cas_gender_county_df %>% pivot_longer(
  cols = 'Baringo':'West.Pokot',
  names_to = "County",
  values_to = "Count")

all_cas_gender_county_df_long_wide <- all_cas_gender_county_df_long %>% pivot_wider(
  names_from = "Gender",
  values_from = "Count") %>%
  filter(County != "Not.Indicated")%>%
  mutate(gender_ratio = round(Female/Male, 2)) %>%
  mutate(County = ifelse(County == "Elgeyo.Marakwet", "Elgeyo Marakwet",
                         ifelse(County == "Taita.Taveta", "Taita Taveta",
                                ifelse(County == "Uasin.Gishu", "Uasin Gishu",
                                       ifelse(County == "West.Pokot","West Pokot",
                                              ifelse(County == "Homa.Bay","Homa Bay",
                                                     ifelse(County == "Tharaka.Nithi","Tharaka Nithi",
                                                            ifelse(County == "Trans.Nzoia", "Trans Nzoia",
                                                                   ifelse(County == "Murang.a", "Murang'a", 
                                                                          ifelse(County == "Tana.River", "Tana River", County))))))))))



all_cas_gender_county_df_long_wide %>%  
  ggplot(aes(reorder(County, gender_ratio), gender_ratio, fill = gender_ratio)) +
  geom_point(size = 3, aes(color = gender_ratio)) +
  geom_segment(aes(x = County, xend = County, y = 0, yend = gender_ratio, color = gender_ratio), size = 1) +
  coord_flip() + 
  theme_classic() + 
  scale_color_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  labs(x = "County", 
       y = "Ratio of females to males", 
       title = "All CAS Applicants",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 15, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

ggsave("images/all_cas_gender_county_ratio.jpg", width = 8, height = 8)

# g) What is the ratio of males to females that were shortlisted per county?

stl_cas_gender_county_df <- data.frame(tabyl(stl_cas, Gender, County))

stl_cas_gender_county_df_long <- stl_cas_gender_county_df %>% pivot_longer(
  cols = 'Baringo':'West.Pokot',
  names_to = "County",
  values_to = "Count")

stl_cas_gender_county_df_long_wide <- stl_cas_gender_county_df_long %>% pivot_wider(
  names_from = "Gender",
  values_from = "Count") %>%
  mutate(gender_ratio = round(Female/Male, 2)) %>%
  mutate(County = ifelse(County == "Elgeyo.Marakwet", "Elgeyo Marakwet",
                         ifelse(County == "Taita.Taveta", "Taita Taveta",
                                ifelse(County == "Uasin.Gishu", "Uasin Gishu",
                                       ifelse(County == "West.Pokot","West Pokot",
                                              ifelse(County == "Homa.Bay","Homa Bay",
                                                     ifelse(County == "Tharaka.Nithi","Tharaka Nithi",
                                                            ifelse(County == "Trans.Nzoia", "Trans Nzoia",
                                                                   ifelse(County == "Murang.a", "Murang'a", 
                                                                          ifelse(County == "Tana.River", "Tana River", County))))))))))


stl_cas_gender_county_df_long_wide %>%  
  ggplot(aes(reorder(County, gender_ratio), gender_ratio, fill = gender_ratio)) +
  geom_point(size = 3, aes(color = gender_ratio)) +
  geom_segment(aes(x = County, xend = County, y = 0, yend = gender_ratio, color = gender_ratio), size = 1) +
  coord_flip() + 
  theme_classic() + 
  scale_color_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, 0.5)) +
  labs(x = "County", 
       y = "Ratio of females to males", 
       title = "Shortlisted CAS Applicants",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 15, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) 

ggsave("images/stl_cas_gender_county_ratio.jpg", width = 8, height = 8)


