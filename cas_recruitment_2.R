# An analysis of the CAS recruitment in Kenya (Part 2)

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

# SHORTLIST

# 1) Load the required data, sort, and review the structure

# All the applicant data
stl_cas <- read_excel("CAS_2.xlsx")
stl_cas <- stl_cas %>%
  mutate(Gender = ifelse(Gender == "F", "Female",
                       ifelse(Gender == "M", "Male", Gender)))

# All the applicant data by gender
stl_cas_gender_df <- data.frame(tabyl(stl_cas, Gender))
str(stl_cas_gender_df)
summary(stl_cas_gender_df)
unique(stl_cas_gender_df)

# All the applicant data by county
stl_cas_county_df <- data.frame(tabyl(stl_cas, County))
str(stl_cas_county_df)
summary(stl_cas_county_df)
unique(stl_cas_county_df)

# 2) Plot the data

# a) After the shortlisting, what is the gender distribution of applicants selected for vetting?

stl_cas_gender_df %>%  
  ggplot(aes(x="", y = n, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar("y", start=0) +
  theme_void() + 
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
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 15),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))  

ggsave("images/stl_cas_gender.jpg", width = 6, height = 4)

# b) After the shortlisting, what is the county of origin for applicants selected for vetting?

stl_cas_county_df %>%  
  ggplot(aes(reorder(County, n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = "County", 
       y = "Number of Shortlisted Applicants", 
       title = "County of Origin for Shortlisted Applicants",
       subtitle = "Distribution of shortlisted applicants by their counties of origin",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 17.5),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 20, face = "bold"),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 17.5, face = "bold"),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) 

ggsave("images/stl_cas_county.jpg", width = 12, height = 12)

