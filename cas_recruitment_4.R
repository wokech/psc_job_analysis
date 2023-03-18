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

# YIELDS

# a) County yields (ratio of shortlisted to all applicants)

# Perform joins to combine the datasets

stl_all_yield <- full_join(all_cas_county_df, stl_cas_county_df, by = "County")

# Drop the % columns, rename the n columns, and drop the "not indicated."

stl_all_yield_2 <- stl_all_yield %>%
  subset(select = -c(percent.x, percent.y)) %>%
  rename("all_apps" = "n.x",
         "short_apps" = "n.y") %>%
  mutate(short_apps = coalesce(short_apps, 0)) %>%
  mutate(yield = (short_apps/all_apps)*100) %>%
  filter(County != "Not Indicated")

stl_all_yield_2$yield <- round(stl_all_yield_2$yield, 2)

stl_all_yield_2 %>%  
  ggplot(aes(reorder(County, yield), yield, fill = yield)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, 2)) +
  labs(x = "County", 
       y = "Percentage (%) of applicants\nthat were shortlisted per County", 
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

ggsave("images/stl_all_yield_2.jpg", width = 8, height = 8)


