# An analysis of the CAS recruitment in Kenya (Part 1)

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

# APPLICANTS

# 1) Load the required data, sort, and review the structure

# All the applicant data
all_cas <- read_excel("CAS_1.xlsx")

# All the applicant data by gender
all_cas_gender_df <- data.frame(tabyl(all_cas, Gender))
str(all_cas_gender_df)
summary(all_cas_gender_df)
unique(all_cas_gender_df)

# All the applicant data by county
all_cas_county_df <- data.frame(tabyl(all_cas, County))
str(all_cas_county_df)
summary(all_cas_county_df)
unique(all_cas_county_df)

# 2) Plot the data

# a) Gender distribution for the CAS applicants position

all_cas_gender_df %>%  
  ggplot(aes(reorder(Gender, -n), n, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  theme_minimal() + 
  scale_fill_manual(values = c("#f4c2c2", "#89CFF0")) +
  labs(x = "", 
       y = "", 
       title = "",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  geom_text(aes(label=comma(n)),color="black",size=10,position=position_stack(vjust=0.5)) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_blank(),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

ggsave("images/all_cas_gender.jpg", width = 6, height = 4)

# b) What counties do the applicants for the CAS position come from?

all_cas_county_df %>%  
  ggplot(aes(reorder(County, n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "#eecda3", high = "#ef629f") +
  labs(x = "County", 
       y = "Number of Total Applicants", 
       title = "",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "right",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) 

ggsave("images/all_cas_county.jpg", width = 8, height = 8)


