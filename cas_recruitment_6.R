# An analysis of the CAS recruitment in Kenya (Part 6)

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

# TREEMAPS

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
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")

# With CAS's equal to 1
nom_cas_county_df_3 <- nom_cas_county_df %>%
  filter(n == 1)

ggplot(nom_cas_county_df_3, aes(area = n, fill = County,
                                label = paste(County, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")

# With no CAS's

# List the counties with no CAS

