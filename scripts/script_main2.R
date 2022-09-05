### load packages ----
library(tidyverse)
library(remotes)
library(devtools)
library(car)
library(dplyr)
install_github("mdelacre/Routliers")

### load dataset, make political affilication and exp. conditions factors, recode political affiliation----
main2 <- read_csv("data/main2_working_data.csv") %>%
  as_tibble() %>%
  mutate(affiliation = as.factor(affiliation),
         affiliation = dplyr::recode(affiliation,
                                   "1" = "Left",
                                   "2" = "Centre",
                                   "3" = "Right",
                                   "4" = "Not affiliated")) %>%
  mutate(gender = dplyr::recode(gender,
                              "1" = "Female",
                              "2" = "Male",
                              "3" = "Trans Female/ Trans Woman",
                              "4" = "Trans Male/ Trans Man",
                              "5" = "Genderqueer/ Gender Non Confirming",
                              "6" = "Different Identity",
                              "7" = "Rather not say")) %>%
  mutate(condition = as.factor(condition))

### subsetting dataset ----

main2_sub <- main2 %>%
  select(id, condition, gender, age, affiliation, eff_check1, eff_check2, hum_check1, hum_check2, selfcat_1, selfcat_2, orgaeff_1, orgaeff_2, stereo_1, stereo_2, legit_1, legit_2, legit_3, support)

### mean age of participants ----
mean(main2_sub$age)
sd(main2_sub$age)

### gender distribution----
main2_sub %>%
  group_by(gender) %>%
  summarise(n())

### mean age and no. of participants per affiliation ----
main2_sub %>%
  group_by(affiliation) %>%
  summarise(., mean_age = mean(age), sd_age = sd(age), n = n())

### gender by political affiliation----
main2_sub %>%
  group_by(affiliation, gender) %>%
  summarise(n())







