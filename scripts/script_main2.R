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

### allocation to conditions----
main2_sub %>%
  group_by(condition) %>%
  summarise(n())

### allocation to conditions (by political affiliation)----
main2_sub %>%
  group_by(condition, affiliation) %>%
  summarise(n())

### manipulation checks----
## humour

main2_sub %>%
  filter(condition == "2") %>%
  summarise(mean_hum_check1 = mean(hum_check1), sd_hum_check1 = sd(hum_check1),
            mean_hum_check2 = mean(hum_check2), sd_hum_check2 = sd(hum_check2))

t.test(main2_sub$hum_check1, mu = 4.32, alternative = "two.sided") # testing whether personal and other humour perception sign. diff 
t.test(main2_sub$hum_check1, mu = 3.5, alternative = "two.sided") # testing whether personal humour perception sign. diff from scale mid-point (3.5)
t.test(main2_sub$hum_check2, mu = 3.5, alternative = "two.sided") # testing whether other humour perception sign. diff from scale mid-point (3.5)

## comparison of perceptions of free expression and organisation across conditions

main2_sub %>%
  group_by(condition) %>%
  summarise(mean_eff_check1 = mean(eff_check1), sd_eff_check1 = sd(eff_check1), med_eff_check1 = median(eff_check1),
            mean_eff_check2 = mean(eff_check2), sd_eff_check2 = sd(eff_check2), med_eff_check2 = median(eff_check2), 
            n = n())

# anova eff_check 1 (= "free expreesion")
eff_1.aov <- aov(eff_check1 ~ condition, data = main2_sub)
summary(eff_1.aov)

TukeyHSD(eff_1.aov) # pairwise comparisons (control vs exp1 and exp2 sign. diff, but among exp no sign. diff)

leveneTest(eff_check1 ~ condition, data = main2_sub) # checking homogeneity of variance: homogeneity of variance is violated, using Welch test instead:

oneway.test(eff_check1 ~ condition, data = main2_sub)

eff_1.pairwise.t.test <- pairwise.t.test(main2_sub$eff_check1, main2_sub$condition,
                                         p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
eff_1.pairwise.t.test # same as above: (control vs exp1 and exp2 sign. diff, but among exp no sign. diff)

plot(eff_1.aov, 2) # checking normality: normality seems violated, therefore, non-parametric test:

kruskal.test(eff_check1 ~ condition, data = main2_sub)

eff_1.pairwise.wilcox.test <- pairwise.wilcox.test(main2_sub$eff_check1, main2_sub$condition,
                                                   p.adjust.method = "BH", conf.int = TRUE) # pairwise comparison (non-parametric)
eff_1.pairwise.wilcox.test # same as above: (control vs exp1 and exp2 sign. diff, but among exp no sign. diff)

# anova eff_check 2 (= "successful rally")
eff_2.aov <- aov(eff_check2 ~ condition, data = main2_sub)
summary(eff_2.aov)

TukeyHSD(eff_2.aov) # pairwise comparisons (same as above: sign. diff. between control and exp1 and 2, but not among exp conditions)

leveneTest(eff_check2 ~ condition, data = main2_sub) # checking homogeneity of variance: no viiolation

plot(eff_2.aov, 2) # checking normality: normality seems violated, therefore, non-parametric test:

kruskal.test(eff_check2 ~ condition, data = main2_sub)

eff_2.pairwise.wilcox.test <- pairwise.wilcox.test(main2_sub$eff_check2, main2_sub$condition,
                                                   p.adjust.method = "BH", conf.int = TRUE) # pairwise comparison (non-parametric)
eff_2.pairwise.wilcox.test # same as above

### reliability & making new variables----

## self-categorization ----

library(performance) # inter-item correlation

interitem_selfcat <- main2_sub[, c("selfcat_1", "selfcat_2")]
item_intercor(interitem_selfcat) # inter-item correlation of 0.93

main2_sub <- main2_sub %>% # create new variable "selfcat"
  mutate(
    selfcat = (selfcat_1 + selfcat_2)/2
  )

## organizational efficacy ----

interitem_orgaeff <- main2_sub[, c("orgaeff_1", "orgaeff_2")]
item_intercor(interitem_orgaeff) # inter-item correlation of 0.91

main2_sub <- main2_sub %>% # create new variable "selfcat"
  mutate(
    orgaeff = (orgaeff_1 + orgaeff_2)/2
  )

## competence stereotype ----

interitem_stereo <- main2_sub[, c("stereo_1", "stereo_2")]
item_intercor(interitem_stereo) # inter-item correlation of 0.81

main2_sub <- main2_sub %>% # create new variable "selfcat"
  mutate(
    stereo = (stereo_1 + stereo_2)/2
  )

## legitimacy (reliablity, validty and variable generation) ----

library(psych) # cronbach's alpha

key <- list(
  legit = c("legit_1", "legit_2", "legit_3")
)

score.items(key, main2_sub) # reliability of 0.88

main2_sub <- main2_sub %>% # create new variable "selfcat"
  mutate(
    legit = (legit_1 + legit_2 + legit_3)/3
  )

library(lavaan) # validity
library(semPlot)
library(lm.beta)

legit.cfa <- 'legit.cfa =~ legit_1 + legit_2 + legit_3'
cfa_legit.sem <- sem(legit.cfa, data = main2_sub)
lavaan::summary(cfa_legit.sem, standardized = TRUE, fit.measures = TRUE) 
# # model is saturated (sign chi-square, RMSEA = 0, CFI = 1)
# the adequacy of saturated models can be tested by experimentally targeting it, i.e., if its predictions match the observed 
# differences (or lack thereof) of the parameter estimates, then the model may be valid 
# (https://stats.stackexchange.com/questions/283/what-is-a-saturated-model#:~:text=If%20a%20model%20is%20saturated,that%20the%20model%20is%20valid.)
# --> can we observe differences in predictions based on condition (i.e., theoretically, individuals in control group should rate the
# the group as legitimate, whereas individuals in the exp conditions should not/ to a lesser degree)

