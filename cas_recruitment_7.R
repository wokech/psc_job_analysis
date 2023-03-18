# An analysis of the CAS recruitment in Kenya (Part 7)

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

# WORDCLOUD

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

word_count_male <- all_cas_split_name_male %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count_male$first_name, freq = word_count_male$n,
          max.words=100, random.order=FALSE)


# Females
all_cas_split_name_female <- all_cas_split_name %>%
  filter(Gender == "Female")

word_count_female <- all_cas_split_name_female %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count_female$first_name, freq = word_count_female$n,
          max.words=100, random.order=FALSE)

# i) Split the name column into first, middle, and last names for 
# shortlisted applicants and tabulate

stl_cas_split_name <- separate(stl_cas, col = Name, into = c("first_name", "middle_name", "last_name"), sep = " ")
data.frame(tabyl(stl_cas_split_name, first_name))

################## Review

word_count_stl <- stl_cas_split_name %>%
  count(first_name, sort = TRUE) %>% 
  mutate(first_name = reorder(first_name, n)) %>%
  ungroup()

wordcloud(words = word_count$first_name, freq = word_count$n,
          max.words=100, random.order=FALSE)
