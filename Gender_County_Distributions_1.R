# An analysis of the CAS recruitment in Kenya

# Notes:
# 1) Gradients - https://digitalsynopsis.com/design/beautiful-color-ui-gradients-backgrounds/
# 2) https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

# (A) Sex/Gender distribution of the applicants for the CAS position in Kenya

# 1. Load the required data

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


all_cas <- read_excel("CAS_1.xlsx")
str(all_cas)

all_cas$Gender <- as.factor(all_cas$Gender)
all_cas$County <- as.factor(all_cas$County)

# 2. Plot the required data

# a) What is the gender distribution of all the applicants for the CAS position?

summary(all_cas)
unique(all_cas$County)
unique(all_cas$Gender)

all_cas_gender_df <- data.frame(tabyl(all_cas, Gender))

str(all_cas_gender_df)

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

all_cas_county_df <- data.frame(tabyl(all_cas, County))
str(all_cas_county_df)

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

# c) After the shortlisting, what is the gender distribution of applicants selected for vetting?

stl_cas_gender_df <- data.frame(tabyl(stl_cas, Gender))

str(stl_cas_gender_df)

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

# d) After the shortlisting, what is the county of origin for applicants selected for vetting?

stl_cas_county_df <- data.frame(tabyl(stl_cas, County))
str(stl_cas_county_df)

stl_cas_county_df %>%  
  ggplot(aes(reorder(County, n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = "County", 
       y = "Number of Shortlisted Applicants", 
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

ggsave("images/stl_cas_county.jpg", width = 8, height = 8)

# e) County yields (ratio of shortlisted to all applicants)

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

# f) What is the ratio of males to females that applied per county?

all_cas_gender_county_df <- data.frame(tabyl(all_cas, Gender, County))

all_cas_gender_county_df_long <- all_cas_gender_county_df %>% pivot_longer(
  cols = 'Baringo':'West.Pokot',
  names_to = "County",
  values_to = "Count")

all_cas_gender_county_df_long_wide <- all_cas_gender_county_df_long %>% pivot_wider(
  names_from = "Gender",
  values_from = "Count") %>%
  filter(County != "Not.Indicated")%>%
  mutate(gender_ratio = round(Female/Male, 2)) %>%
  mutate(County = ifelse(County == "Elgeyo.Marakwet", "Elgeyo Marakwet",
                         ifelse(County == "Taita.Taveta", "Taita Taveta",
                                ifelse(County == "Uasin.Gishu", "Uasin Gishu",
                                       ifelse(County == "West.Pokot","West Pokot",
                                              ifelse(County == "Homa.Bay","Homa Bay",
                                                     ifelse(County == "Tharaka.Nithi","Tharaka Nithi",
                                                            ifelse(County == "Trans.Nzoia", "Trans Nzoia",
                                                                   ifelse(County == "Murang.a", "Murang'a", 
                                                                          ifelse(County == "Tana.River", "Tana River", County))))))))))



all_cas_gender_county_df_long_wide %>%  
  ggplot(aes(reorder(County, gender_ratio), gender_ratio, fill = gender_ratio)) +
  geom_point(size = 3, aes(color = gender_ratio)) +
  geom_segment(aes(x = County, xend = County, y = 0, yend = gender_ratio, color = gender_ratio), size = 1) +
  coord_flip() + 
  theme_classic() + 
  scale_color_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  labs(x = "County", 
       y = "Ratio of females to males", 
       title = "All CAS Applicants",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 15, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white"))

ggsave("images/all_cas_gender_county_ratio.jpg", width = 8, height = 8)

# g) What is the ratio of males to females that were shortlisted per county?

stl_cas_gender_county_df <- data.frame(tabyl(stl_cas, Gender, County))

stl_cas_gender_county_df_long <- stl_cas_gender_county_df %>% pivot_longer(
  cols = 'Baringo':'West.Pokot',
  names_to = "County",
  values_to = "Count")

stl_cas_gender_county_df_long_wide <- stl_cas_gender_county_df_long %>% pivot_wider(
  names_from = "Gender",
  values_from = "Count") %>%
  mutate(gender_ratio = round(Female/Male, 2)) %>%
  mutate(County = ifelse(County == "Elgeyo.Marakwet", "Elgeyo Marakwet",
                         ifelse(County == "Taita.Taveta", "Taita Taveta",
                                ifelse(County == "Uasin.Gishu", "Uasin Gishu",
                                       ifelse(County == "West.Pokot","West Pokot",
                                              ifelse(County == "Homa.Bay","Homa Bay",
                                                     ifelse(County == "Tharaka.Nithi","Tharaka Nithi",
                                                            ifelse(County == "Trans.Nzoia", "Trans Nzoia",
                                                                   ifelse(County == "Murang.a", "Murang'a", 
                                                                          ifelse(County == "Tana.River", "Tana River", County))))))))))


stl_cas_gender_county_df_long_wide %>%  
  ggplot(aes(reorder(County, gender_ratio), gender_ratio, fill = gender_ratio)) +
  geom_point(size = 3, aes(color = gender_ratio)) +
  geom_segment(aes(x = County, xend = County, y = 0, yend = gender_ratio, color = gender_ratio), size = 1) +
  coord_flip() + 
  theme_classic() + 
  scale_color_gradient(low = "#eecda3", high = "#ef629f") +
  scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3.2, 0.5)) +
  labs(x = "County", 
       y = "Ratio of females to males", 
       title = "Shortlisted CAS Applicants",
       subtitle = "",
       caption = "By @willyokech\nData Source: PSC (publicservice.go.ke)",
       fill = "")+
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 15, vjust = 1),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) 

ggsave("images/stl_cas_gender_county_ratio.jpg", width = 8, height = 8)

# h) Split the name column into first, middle, and last names for 
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
