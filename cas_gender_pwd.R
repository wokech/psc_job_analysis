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
#install.packages("ggrepel")
library(ggrepel)

#################################################################################
# PYRAMID PLOT
#################################################################################

# Perform joins to combine the total, shortlist, and nominated datasets

all_stl_gender <- full_join(all_cas_gender_df, stl_cas_gender_df, by = "Gender")
all_stl_nom_gender <- full_join(all_stl_gender, nom_cas_gender_df, by = "Gender")

# Drop the % columns, rename the n columns, and drop the "not indicated."

all_stl_nom_gender_2 <- all_stl_nom_gender %>%
  subset(select = -c(percent.x, percent.y, percent)) %>%
  rename("all" = "n.x",
         "shortlist" = "n.y",
         "nominate" = "n") 

all_stl_nom_gender_2 <- all_stl_nom_gender_2 %>%
  pivot_longer(!Gender, names_to = "stage", values_to = "population")

all_stl_nom_gender_2 %>%
ggplot(aes(x = reorder(stage, population), fill = Gender,
                 y = ifelse(test = Gender == "Male",
                            yes = -population, no = population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(all_stl_nom_gender_2$population) * c(-1,1)) +
  scale_x_discrete(labels=c("all" = "Applied", "shortlist" = "Shortlisted",
                              "nominate" = "Nominated")) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#f4c2c2", "#89CFF0")) +
  labs(x = "Application Stage", 
       y = "Number", 
       title = "Only 26% of CAS appointees are women",
       subtitle = "Distribution of males and females at the application, shortlisting, and nomination stages",
       caption = "Data Source: PSC (publicservice.go.ke) | By @willyokech",
       fill = "") +
  geom_text(x=3, y=3100, color = "#f4c2c2", size = 7, family= "Helvetica", fontface="bold", label="33% Female") +
  geom_text(x=2, y=1200, color = "#f4c2c2", size = 7, family =  "Helvetica", fontface="bold", label="36% Female") +
  geom_text(x=1, y=1200, color = "#f4c2c2", size = 7, family = "Helvetica", fontface="bold", label="26% Female") +
  geom_label_repel(aes(label=comma(population)),color="black", size = 7, nudge_x = 0, nudge_y = 10, show.legend = FALSE) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 25),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        legend.title = element_text("Helvetica",size = 1, vjust = 1),
        legend.position = "none",
        legend.text=element_text(size=15),
        plot.caption = element_text(family = "Helvetica",size = 15, face = "bold"),
        plot.background = element_rect(fill = "beige", colour = "beige"),
        panel.background = element_rect(fill = "beige", colour = "beige")) 

ggsave("images/app_stl_nom_cas_gender.jpg", width = 12, height = 8)

#################################################################################
# PWD
#################################################################################

all_cas_pwd <- all_cas %>% 
  filter(str_detect(Name, "PWD")) %>%
  nrow()
stl_cas_pwd <- stl_cas %>% 
  filter(str_detect(Name, "PWD")) %>%
  nrow()
nom_cas_pwd <- nom_cas %>% 
  filter(str_detect(Name, "PWD")) %>%
  nrow()

# Create a data frame
pwd <- data.frame(app_stage = c("Applied", "Shortlisted", "Nominated"),
                  number = c(all_cas_pwd, stl_cas_pwd, nom_cas_pwd))

pwd %>% 
  ggplot(aes(x = reorder(app_stage, -number), number, fill = app_stage)) + 
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_fill_grey() +
  ylim(0,80) +
  labs(x = "Application Stage", 
       y = "Number", 
       title = "One out of the 50 CAS appointees is a person with a disability (PWD)",
       subtitle = "Distribution of PWDs at the application, shortlisting, and nomination stages",
       caption = "Data Source: PSC (publicservice.go.ke) | By @willyokech",
       fill = "")+
  geom_text(aes(label=comma(number)),color="black", size = 8, vjust = -0.5, position = position_dodge(0.9)) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 25),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        legend.title = element_text("Helvetica",size = 1, vjust = 1),
        legend.position = "none",
        legend.text=element_text(size=15),
        plot.caption = element_text(family = "Helvetica",size = 15, face = "bold"),
        plot.background = element_rect(fill = "beige", colour = "beige"),
        panel.background = element_rect(fill = "beige", colour = "beige"))

ggsave("images/pwd.jpg", width = 12, height = 8)

