# An analysis of the CAS recruitment in Kenya (Part 4)

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
library(patchwork)

##################################################################################
# WORDCLOUD OF NAMES
##################################################################################

# a) Split the name column into first, middle, and last names for 
# all applicants and tabulate

all_cas_split_name <- separate(all_cas, col = Name, into = c("first_name", "middle_name", "last_name"), sep = " ")
data.frame(tabyl(all_cas_split_name, first_name))

word_count_all <- all_cas_split_name %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count_all$first_name, freq = word_count_all$n,
          max.words=100, random.order=FALSE)

# Males
all_cas_split_name_male <- all_cas_split_name %>%
  filter(Gender == "Male")

# Get word count for names with greater than 20 occurrences
word_count_male <- all_cas_split_name_male %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  filter(n >= 20) %>%
  ungroup()

set.seed(42)
p1 <- ggplot(word_count_male, aes(label = first_name, size = n)) +
  geom_text_wordcloud(shape = "triangle-upright") +
  scale_size_area(max_size = 20) +
  labs(title = "MALE",
       subtitle = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 15, face = "bold"),
        plot.background = element_rect(fill = "beige", colour = "beige"),
        panel.background = element_rect(fill = "beige", colour = "beige"))

ggsave("images/all_cas_first_name_male.png", width = 6, height = 4)

# Females
all_cas_split_name_female <- all_cas_split_name %>%
  filter(Gender == "Female")

word_count_female <- all_cas_split_name_female %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  filter(n >= 20) %>%
  ungroup()

set.seed(42)
p2 <- ggplot(word_count_female, aes(label = first_name, size = n)) +
  geom_text_wordcloud(shape = "triangle-upright") +
  scale_size_area(max_size = 20) +
  labs(title = "FEMALE",
       subtitle = "",
       caption = "",
       fill = "") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 15, face = "bold"),
        plot.background = element_rect(fill = "beige", colour = "beige"),
        panel.background = element_rect(fill = "beige", colour = "beige")) 
p2
ggsave("images/all_cas_first_name_female.png", width = 6, height = 4)

p1 + p2 + 
  plot_annotation(title = "Esther and John were the most common applicant first names",
                  subtitle = "Visualization of applicant first names with greater than 20 occurrences",
                  caption = "Data Source: PSC (publicservice.go.ke) | By: @willyokech\n*Word size is proportional to name frequency",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 15, face = "bold"),
                                plot.background = element_rect(fill = "beige"))) &
  theme(text = element_text('Helvetica'))

ggsave("images/all_cas_first_name.png", width = 12, height = 8)
  

# b) Split the name column into first, middle, and last names for 
# shortlisted applicants and tabulate

stl_cas_split_name <- separate(stl_cas, col = Name, into = c("first_name", "middle_name", "last_name"), sep = " ")
data.frame(tabyl(stl_cas_split_name, first_name))

word_count_stl <- stl_cas_split_name %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count_stl$first_name, freq = word_count_stl$n,
          max.words=100, random.order=FALSE)

# b) Split the name column into first, middle, and last names for 
# nominated applicants and tabulate

nom_cas_split_name <- separate(nom_cas, col = Name, into = c("first_name", "middle_name", "last_name"), sep = " ")
data.frame(tabyl(nom_cas_split_name, first_name))

word_count_nom <- nom_cas_split_name %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count_nom$first_name, freq = word_count_nom$n,
          max.words=100, random.order=FALSE)
