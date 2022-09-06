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

### descriptive statistics----

main2_sub %>%
  select(selfcat, orgaeff, stereo, legit, support) %>%
  psych::describe() %>%
  as_tibble(rownames="rowname")  %>%
  print()

# selfcat: negative kurtosis but within boundaries, slight positive skew; orgaeff, stereo, legit: negative kurtosis but within boundaries; 
# support: positive skew --> rejection of support, measures in further analyses: bootstrap against violations to normality

summary_descriptives <- main2_sub %>%
  dplyr::group_by(condition) %>%
  summarise(
    selfcat_min = min(selfcat),
    selfcat_max = max(selfcat),
    selfcat_mean = mean(selfcat),
    selfcat_sd = sd(selfcat),
    orgaeff_min = min(orgaeff),
    orgaeff_max = max(orgaeff),
    orgaeff_mean = mean(orgaeff),
    orgaeff_sd = sd(orgaeff),
    stereo_min = min(stereo),
    stereo_max = max(stereo),
    stereo_mean = mean(stereo),
    stereo_sd = sd(stereo),
    legit_min = min(legit),
    legit_max = max(legit),
    legit_mean = mean(legit),
    legit_sd = sd(legit),
    support_min = min(support),
    support_max = max(support),
    support_mean = mean(support),
    support_sd = sd(support))
summary

## univariate outliers inspection by variable----

# selfcat
selfcat_out <- main2_sub$selfcat
selfcat_out_mad <- Routliers::outliers_mad(x=selfcat_out)
selfcat_out_mad # no outliers detected

# orgaeff
orgaeff_out <- main2_sub$orgaeff
orgaeff_out_mad <- Routliers::outliers_mad(x=orgaeff_out)
orgaeff_out_mad # no outliers detected

# stereo
stereo_out <- main2_sub$stereo
stereo_out_mad <- Routliers::outliers_mad(x=stereo_out)
stereo_out_mad # no outliers detected

# legit
legit_out <- main2_sub$legit
legit_out_mad <- Routliers::outliers_mad(x=legit_out)
legit_out_mad # no outliers detected

# support
support_out <- main2_sub$support
support_out_mad <- Routliers::outliers_mad(x=support_out)
support_out_mad # 15 outliers detected (extremely high)

outliers_support <- dplyr::filter(main2_sub, support >= "6.4478")
outliers_support #IDs: 35, 42, 71, 115, 138, 151, 165, 263, 289, 293, 301, 346, 358, 374, 429: 
# 9 out of 15 right affiliated, 2 NA, 2 centre, 2 left, 9 men, 6 in exp2, 6 in exp1, 3 in control
# most important: high self-categorisation as possible explanation 
# (13 out of 15 have a selfcat score of 7, one of 6, and one of 4.5)

## comparison variables across conditions----

orgaeff.anova <- aov(orgaeff ~ condition, data = main2_sub) # sign.
summary(orgaeff.anova)
TukeyHSD(orgaeff.anova) # sign. between exp and control but not between exp

leveneTest(orgaeff ~ condition, data = main2_sub) # n.s.

orgaeff_condition <- main2_sub%>% select(condition, orgaeff) %>% plot()

stereo.anova <- aov(stereo ~ condition, data = main2_sub) # sign.
summary(stereo.anova)
TukeyHSD(stereo.anova) # sign. between exp and control but not between exp

leveneTest(stereo ~ condition, data = main2_sub) # n.s.

stereo_condition <- main2_sub%>% select(condition, stereo) %>% plot()

legit.anova <- aov(legit ~ condition, data = main2_sub) # n.s.
summary(legit.anova)

leveneTest(legit ~ condition, data = main2_sub) # n.s.

legit_condition <- main2_sub%>% select(condition, legit) %>% plot()

support.anova <- aov(support ~ condition, data = main2_sub) # n.s.
summary(support.anova) # n.s.

leveneTest(support ~ condition, data = main2_sub) # n.s.

support_condition <- main2_sub%>% select(condition, support) %>% plot() # saddest graph

### categorical relationships

selfcat_affiliation <- main2_sub %>%
  dplyr::group_by(affiliation) %>%
  summarise(selfcat_mean = mean(selfcat),
            selfcat_sd = sd(selfcat))
selfcat_affiliation # left: M = 2.26, SD = 1.32; Centre: M = 3.62, SD = 1.47; Right: M = 4.67, SD = 1.54; NA: M = 3.54, SD = 1.35

support_affiliation <- main2_sub%>% select(affiliation, support) %>% plot()

support_affiliation_anova <- aov(support ~ affiliation, data = main2_sub)
summary(support_affiliation_anova)
TukeyHSD(support_affiliation_anova) # all despite one (NA/ centre) contrasts sign., biggest diff. between left/right

### continous relationships
## intercorrelations----

main2_cor <-main2_sub %>%
  select(selfcat, orgaeff, stereo, legit, support) %>%
  round(., 2)

library(Hmisc)
rcorr(as.matrix(main2_cor)) %>%
  print()

library(apaTables)
apa.cor.table(main2_cor,filename = "Correlation_main2.doc",table.number = 1,show.conf.interval = F)

## multivariate outliers----

# support/selfcat
support <- main2_sub$support
selfcat <- main2_sub$selfcat
support_selfcat_mcd <- Routliers::outliers_mcd(x = data.frame(support, selfcat))
support_selfcat_mcd # 158 outliers detected
Routliers::plot_outliers_mcd(support_selfcat_mcd, x = data.frame(support, selfcat))


