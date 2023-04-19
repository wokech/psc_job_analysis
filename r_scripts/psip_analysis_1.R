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
library(janitor)

# APPLICANTS

# 1) Load the required data, sort, and review the structure

# All the PSIP data

psip_2019_2020 <- read_excel("PSIP/PSIP_2019_2020.xlsx")
psip_2020_2021 <- read_excel("PSIP/PSIP_2020_2021.xlsx")
psip_2021_2022 <- read_excel("PSIP/PSIP_2021_2022.xlsx")
psip_2022_2023 <- read_excel("PSIP/PSIP_2022_2023.xlsx")

psip_2019_2020 <- psip_2019_2020 %>% clean_names()
psip_2020_2021 <- psip_2020_2021 %>% clean_names()
psip_2021_2022 <- psip_2021_2022 %>% clean_names()
psip_2022_2023 <- psip_2022_2023 %>% clean_names()

# Gender data

psip_2019_2020_gender_df <- data.frame(tabyl(psip_2019_2020, gender))
psip_2020_2021_gender_df <- data.frame(tabyl(psip_2020_2021, gen))
psip_2021_2022_gender_df <- data.frame(tabyl(psip_2021_2022, gender))
psip_2022_2023_gender_df <- data.frame(tabyl(psip_2022_2023, gender))

# Education data (2020 - 2021)

psip_2020_2021 <- psip_2020_2021 %>%
  mutate(area_of_specialization = recode(area_of_specialization,
                                         "Mathematics, Actuarial Science &\r\nEconomics" = "Mathematics, Actuarial Science & Economics",
                                         "Textile Technology, Clothing And\r\nFashion Design" = "Textile Technology, Clothing, & Fashion Design",
                                         "Architecture, Design& Planning" = "Architecture, Design & Planning",
                                         "Food Science And Nutrition" = "Food Science & Nutrition",
                                         "Humanities And Social Sciences" = "Humanities & Social Sciences",
                                         "Textile Technology, Clothing And Fashion Design" = "Textile Technology, Clothing, & Fashion Design"))

psip_2020_2021_education_df <- data.frame(tabyl(psip_2020_2021, area_of_specialization))

unique(psip_2020_2021$area_of_specialization)                             

psip_2020_2021_education_df %>%
  ggplot(aes(fct_reorder(area_of_specialization, n), n)) + 
  geom_col(fill = "blue") + 
  coord_flip() +
  labs(x = "Area of specialization",
       y = "Number of interns",
       title = "PSIP interns (2020 - 2021 Cohort)",
       subtitle = "Area of specialization for the PSIP interns") + 
  theme_minimal()

# County data

psip_2019_2020 <- psip_2019_2020 %>% 
  mutate(county = recode(county,
                         "BometCounty" = "Bomet",
                         "ElgeyoMarakwet" = "Elgeyo Marakwet",
                         "HomaBayCounty" = "Homa Bay",
                         "IsioloCounty" = "Isiolo",
                         "KerichoCounty" = "Kericho",
                         "KisiiCounty" = "Kisii",
                         "LaikipiaCounty" = "Laikipia",
                         "MigoriCounty" = "Migori",
                         "MoyaleCounty" = "Moyale",
                         "NandiCounty" = "Nandi",
                         "NyamiraCounty" = "Nyamira",
                         "SamburuCounty" = "Samburu",
                         "TaitaTaveta" = "Taita Taveta",
                         "TanaRiver" = "Tana River",
                         "TharakaNithi" = "Tharaka Nithi",
                         "TransNzoia" = "Trans Nzoia",
                         "UasinGishu" = "Uasin Gishu",
                         "WestPokot" = "West Pokot"
                         ))

psip_2020_2021 <- psip_2020_2021 %>%
  mutate(county = recode(county,
                       "Tharakanithi" = "Tharaka Nithi",
                       "Transnzoia" = "Trans Nzoia"
                       ))

psip_2021_2022  <- psip_2021_2022 %>%
  mutate(county = recode(county,
                         "Homabay" = "Homa Bay",
                         "Tharakanithi" = "Tharaka Nithi",
                         "Transnzoia" = "Trans Nzoia"
  ))

psip_2022_2023 <- psip_2022_2023 %>%
  mutate(county = recode(county,
                         "Elgeyo/Marakwet" = "Elgeyo Marakwet",
                         "Homa Bay Town" = "Homa Bay",
                         "MACHAKOS" = "Machakos",
                         "Masrsabit" = "Marsabit",
                         "Muranga" = "Murang'a",
                         "Taita/Taveta" = "Taita Taveta",
                         "Tharaka-Nithi" = "Tharaka Nithi",
                         "Uasungishu" = "Uasin Gishu"
  ))


psip_2019_2020_county_df <- data.frame(tabyl(psip_2019_2020, county))
psip_2020_2021_county_df <- data.frame(tabyl(psip_2020_2021, county))
psip_2021_2022_county_df <- data.frame(tabyl(psip_2021_2022, county))
psip_2022_2023_county_df <- data.frame(tabyl(psip_2022_2023, county))
