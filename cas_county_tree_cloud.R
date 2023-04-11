# An analysis of the CAS recruitment in Kenya (Part 3)

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

# TREEMAPS AND WORDCLOUDS

# j) Treemap of the counties of nominees

ggplot(nom_cas_county_df, aes(area = n, fill = County,
                              label = paste(County, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") 

# With CAS's greater than 1
nom_cas_county_df_2 <- nom_cas_county_df %>%
  filter(n > 1)

ggplot(nom_cas_county_df_2, aes(area = n, fill = County,
                                label = paste(County, n, sep = "\n"))) +
  geom_treemap() +
  scale_fill_brewer(palette = "Set3")+
  geom_treemap_text(colour = "Black",
                    place = "centre",
                    size = 20) +
  theme(legend.position = "none") +
  labs(title = "More than one CAS nominee",
       subtitle = "A list of counties with more than one CAS nominee",
       caption = "Source: PSC (publicservice.go.ke) | By @willyokech",
       fill = "") +
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

ggsave("images/nom_county_over_1.jpg", width = 12, height = 8)


# With CAS's equal to 1
nom_cas_county_df_3 <- nom_cas_county_df %>%
  filter(n == 1)

ggplot(nom_cas_county_df_3, aes(area = n, fill = County,
                                label = paste(County, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 20) +
  theme(legend.position = "none")

ggsave("images/nom_county_equal_1.jpg", width = 12, height = 8)

# With no CAS's

# List the counties with no CAS

word_count_nom_cas_zero <- nom_cas_county_null %>%
  filter(County != "Not Indicated") %>%
  count(County, sort = TRUE) %>% 
  mutate(County = reorder(County, n)) %>%
  ungroup()

set.seed(42)
ggplot(word_count_nom_cas_zero, aes(label = County)) +
  #geom_text_wordcloud() +
  geom_text_wordcloud_area(color = "#000000", size = 18) +
  theme_minimal() + 
  labs(title = "Zero CAS nominees",
       subtitle = "A list of counties with zero CAS nominees",
       caption = "Source: PSC (publicservice.go.ke) | By @willyokech",
       fill = "") +
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

ggsave("images/nom_county_equal_0.jpg", width = 12, height = 8)
