# (A) Sex/Gender distribution of the applicants for the CAS position in Kenya

# 1. Load the required data

library(tidyverse)
library(janitor)
library(tidyr)
library(readxl)

all_cas <- read_excel("CAS_1.xlsx")
str(all_cas)

all_cas$Gender <- as.factor(all_cas$Gender)
all_cas$County <- as.factor(all_cas$County)

# 2. Plot the required data

summary(all_cas)
unique(all_cas$County)
unique(all_cas$Gender)

all_cas_gender <- all_cas %>%  
  ggplot(aes(fct_infreq(Gender), fill = Gender)) +
  geom_bar() + 
  theme_minimal() +
  labs(x = "Gender", 
       y = "Number of applicants", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

all_cas_gender

all_cas_county <- all_cas %>%  
  ggplot(aes(fct_rev(fct_infreq(County)))) +
  geom_bar(fill = "goldenrod1", width = .5) +
  coord_flip() + 
  theme_classic() +
  labs(x = "County", 
       y = "Number of applicants", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

all_cas_county 


# (B) Sex/Gender distribution of the SHORTLISTED applicants for the CAS position in Kenya

# 1. Load the required data

library(tidyverse)
library(janitor)
library(tidyr)
library(readxl)
library(stringr)

stl_cas <- read_excel("CAS_2.xlsx")
str(stl_cas)

stl_cas$Gender[stl_cas$Gender=="M"]<-"Male"
stl_cas$Gender[stl_cas$Gender=="F"]<-"Female"


stl_cas$Gender <- as.factor(stl_cas$Gender)
stl_cas$County <- as.factor(stl_cas$County)

# 2. Plot the required data

summary(stl_cas)
unique(stl_cas$County)
unique(stl_cas$Gender)

stl_cas_gender <- stl_cas %>%  
  ggplot(aes(fct_infreq(Gender))) +
  geom_bar(fill = "goldenrod4") + 
  theme_minimal() +
  labs(x = "Gender", 
       y = "Number of shortlisted applicants", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

stl_cas_gender

stl_cas_county <- stl_cas %>%  
  ggplot(aes(fct_rev(fct_infreq(County)))) +
  geom_bar(fill = "goldenrod4", width = .5) +
  scale_y_continuous(breaks=seq(0,12,1)) +
  coord_flip() + 
  theme_classic() +
  labs(x = "County", 
       y = "Number of shortlisted applicants", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

stl_cas_county 

library(patchwork)

(all_cas_gender + all_cas_county)  

(stl_cas_gender + stl_cas_county)

# Get the required frequency tables

all_cas_gender_df <- data.frame(tabyl(all_cas, Gender))
all_cas_county_df <- data.frame(tabyl(all_cas, County))
all_cas_gender_county_df <- data.frame(tabyl(all_cas, Gender, County))
stl_cas_gender_df <- data.frame(tabyl(stl_cas, Gender))
stl_cas_county_df <- data.frame(tabyl(stl_cas, County))
stl_cas_gender_county_df <- data.frame(tabyl(stl_cas, Gender, County))


