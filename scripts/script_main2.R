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

# increase deciamls in output (pairwise.wilcox.test) below
print.pairwise.htest <- function (x, digits = 5, ...)
{
  cat("\n\tPairwise comparisons using", x$method, "\n\n")
  cat("data: ", x$data.name, "\n\n")
  pp <- format.pval(x$p.value, digits, na.form = "-")
  attributes(pp) <- attributes(x$p.value)
  print(pp, quote = FALSE, ...)
  cat("\nP value adjustment method:", x$p.adjust.method, "\n")
  invisible(x)
}

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

main2_sub_dsbycond <- main2_sub %>%
  select(condition, selfcat, orgaeff, stereo, legit, support)

library(vtable)

st(main2_sub_dsbycond, group = 'condition', group.test = TRUE)

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
library(effectsize)

orgaeff.anova <- aov(orgaeff ~ condition, data = main2_sub) # sign.
summary(orgaeff.anova)
eta_squared(orgaeff.anova, partial = TRUE)
TukeyHSD(orgaeff.anova) # sign. between exp and control but not between exp

leveneTest(orgaeff ~ condition, data = main2_sub) # n.s.

orgaeff_condition <- main2_sub%>% select(condition, orgaeff) %>% 
  plot(xlab="Experimental condition", ylab="Organisational efficacy", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)") 
 

stereo.anova <- aov(stereo ~ condition, data = main2_sub) # sign.
summary(stereo.anova)
eta_squared(stereo.anova, partial = TRUE)
TukeyHSD(stereo.anova) # sign. between exp and control but not between exp

leveneTest(stereo ~ condition, data = main2_sub) # n.s.

stereo_condition <- main2_sub%>% select(condition, stereo) %>% plot()

legit.anova <- aov(legit ~ condition, data = main2_sub) # n.s.
summary(legit.anova)
eta_squared(legit.anova, partial = TRUE)

leveneTest(legit ~ condition, data = main2_sub) # n.s.

legit_condition <- main2_sub%>% select(condition, legit) %>% 
  plot(xlab="Experimental condition", ylab="Legitimacy", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)")

support.anova <- aov(support ~ condition, data = main2_sub) # n.s.
summary(support.anova) # n.s.
eta_squared(support.anova, partial = TRUE)

leveneTest(support ~ condition, data = main2_sub) # n.s.

support_condition <- main2_sub%>% select(condition, support) %>% 
  plot(xlab="Experimental condition", ylab="Support intention", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)") 

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

main2_sub_numcond <- main2_sub %>%
  mutate(condition = as.numeric(condition))

# correlation with condition

corr_cond_orgaeff <- cor.test(x=main2_sub_numcond$condition, y=main2_sub_numcond$orgaeff, method = 'spearman')
corr_cond_orgaeff # r = -.40, p < .001 --> experimental conditions (1,2) are associated with lower organisational efficacy

corr_cond_stereo <- cor.test(x=main2_sub_numcond$condition, y=main2_sub_numcond$stereo, method = 'spearman')
corr_cond_stereo # r = - .29, p < .001 --> experimental conditions (1,2) are associated with lower competence

corr_cond_legit <- cor.test(x=main2_sub_numcond$condition, y=main2_sub_numcond$legit, method = 'spearman')
corr_cond_legit # r = .00, n.s.

corr_cond_support <- cor.test(x=main2_sub_numcond$condition, y=main2_sub_numcond$support, method = 'spearman')
corr_cond_support # r = .07, n.s.

### filter  and compare participants that were relatively close to guessing the purpose of the experiment----

main2_sub <- main2_sub %>% #new "condition" = "purpose" (= purpose guessed) vs "no purpose" (purposed not guessed)
  mutate(purpose = ifelse(id ==  "2"| id == "9"| id == "81"| id == "82" | id == "97"| id == "210"| id =="218"|
                            id == "249"| id == "260"| id == "286"| id == "291"| id =="293"| id == "301"| id == "311"| 
                            id == "315"|id == "347" |id == "353" |id == "357"|id == "359"|id == "380" 
                            |id == "400", 1, 0)) 
main2_sub %>% 
  group_by(purpose) %>%
  summarise(mean = mean(support), sd = sd(support)) # not guessed: M = 2.67, SD = 1.74; guessed: M = 2.90, SD = 1.87

t.test(main2_sub$support ~ main2_sub$purpose, var.equal = FALSE) # n.s.

## multivariate outliers----

# support/selfcat
support <- main2_sub$support
selfcat <- main2_sub$selfcat
support_selfcat_mcd <- Routliers::outliers_mcd(x = data.frame(selfcat, support))
support_selfcat_mcd # 158 outliers detected
Routliers::plot_outliers_mcd(support_selfcat_mcd, x = data.frame(selfcat, support))

outliers_support_selfcat <- support_selfcat_mcd$outliers_pos
outliers_support_selfcat 

legit <- main2_sub$legit
legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(legit, support))
legit_support_mcd # 157 outliers found
Routliers::plot_outliers_mcd(legit_support_mcd, x = data.frame(legit, support))

outliers_legit_support <- legit_support_mcd$outliers_pos
outliers_legit_support

# above outliers overlap mostly, those that drive support from perceiving the group as legitimate are also those that drive the support based on identifying with the group 

efficacy <- main2_sub$orgaeff
legit_orgaeff_mcd <- Routliers::outliers_mcd(x = data.frame(efficacy, legit))
legit_orgaeff_mcd # 4 outliers
Routliers::plot_outliers_mcd(legit_orgaeff_mcd, x = data.frame(efficacy, legit)) # no substiantial difference in slopes

stereo <- main2_sub$stereo
legit_stereo_mcd <- Routliers::outliers_mcd(x = data.frame(stereo, legit))
legit_stereo_mcd # 7 outliers 
Routliers::plot_outliers_mcd(legit_stereo_mcd, x = data.frame(stereo, legit)) # no substantial difference in slopes

support_orgaeff_mcd <- Routliers::outliers_mcd(x = data.frame(efficacy, support))
support_orgaeff_mcd # 158 outliers
Routliers::plot_outliers_mcd(support_orgaeff_mcd, x = data.frame(efficacy, support)) 

outliers_support_orgaeff <- support_orgaeff_mcd$outliers_pos
outliers_support_orgaeff # overlaps with outliers above --> those that drive support based on perceived efficacy, are also those that drive support from perceiving the group as legitimate and those that drive the support based on identifying with the group 

support_stereo_mcd <- Routliers::outliers_mcd(x = data.frame(stereo, support))
support_stereo_mcd # 158 outliers
Routliers::plot_outliers_mcd(support_stereo_mcd, x = data.frame(stereo, support)) 

outliers_support_stereo <- support_stereo_mcd$outliers_pos
outliers_support_stereo # overlaps with outliers above --> those that drive support based on perceived competence, are also those that drive support from perceiving the group as legitimate and those that drive the support based on identifying with the group

### import process function----

source("scripts/process_function.R")

## mediation analyses (option mcx = 3 automatic Helmert contrast coding; modelbt = 1 robust measures)----

# orgaeff = M1

med_orgaeff <- process(data = main2_sub_numcond, y = "support", x = "condition", m = c("orgaeff", "legit"), modelbt =1, mcx = 3, total = 1, model = 6, boot = 10000, seed = 09922) 

# outlier and assumption checks of med_orgaeff

# Outlier treatment: "First, we will recreate the underlying regressions and inspect each regression for outliers. 
# We will then compare the regressions running them each with and without found outliers. This is to establish whether 
# there are substantial changes between regressions (i.e., changes in significance level and/ or valence) as well as 
# to establish the extent to which – if at all – outliers are influential. We will then, however, compare robust 
# regressions with the PROCESS OLS regressions . If the regressions do not deviate substantially (see above) we 
# take that the PROCESS OLS regressions are robust against potential outliers."

# Model assumptions: "In the analyses, we will apply robust measures (bootstrap) that can treat and prevent potential 
# violations against normality, homoscedasticity, and homogeneity of the variables. We will use scatterplots of the 
# individual regression relationships (see above; reconstruction of regression) to investigate linearity." 

# preparing variables: Helmert coding manually

d1<-(main2_sub_numcond$condition==1)*(-2/3)+(main2_sub_numcond$condition > 1)*(1/3)
d2<-(main2_sub_numcond$condition==2)*(-1/2)+(main2_sub_numcond$condition==3)*(1/2)
main2_sub_numcond <-data.frame(main2_sub_numcond,d1,d2)

helmert = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), ncol = 2)
helmert

main2_sub_numfact <- main2_sub_numcond %>%
  mutate(condition = as.factor(condition)) # contrasts requires condition to be a factor 

contrasts(main2_sub_numfact$condition) = helmert

# regression 1 (orgaeff ~ condition)
orgaeff_cond <- lm(orgaeff ~ condition, data = main2_sub_numfact)
summary(orgaeff_cond)
# deviation from pre-registration: no scatterplots due to categorical data (condition)

orgaeff_cond_cooksd <- cooks.distance(orgaeff_cond)

plot(orgaeff_cond_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(orgaeff_cond_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(orgaeff_cond_cooksd)+1, y=orgaeff_cond_cooksd, labels=ifelse(orgaeff_cond_cooksd>4*mean(orgaeff_cond_cooksd, na.rm=T),names(orgaeff_cond_cooksd),""), col="red")  # add labels

# => 14 outliers: ID 6, 58, 71, 138, 150, 184, 279, 294, 301, 328, 337, 371, 428, 429 

influential <- as.numeric(names(orgaeff_cond_cooksd)[orgaeff_cond_cooksd > 4*mean(orgaeff_cond_cooksd, na.rm=T)])  # influential row numbers
head(main2_sub_numfact[influential, ], n = 20)  # identifying outliers 

car::outlierTest(orgaeff_cond) # most extreme outlier = ID 58 (condition = control, left, male, 52y, scores of "1" throughout)

# run again without outliers 

noutliers1 <- c("6", "58", "71", "138", "150", "184", "279", "294", "301", "328", "337", "371", "428", "429")

main2_sub_numfact_noutliers1 <- main2_sub_numfact %>%
  filter(!id %in% noutliers1)

orgaeff_cond_nout <- lm(orgaeff ~ condition, data = main2_sub_numfact_noutliers1)
summary(orgaeff_cond_nout) # no significant or valence changes: cond1: b = -1.34, p < .001; cond2: b = -.13, p = .35

# robust regression 1

library(robustbase)
orgaeff_cond_rob <- lmrob(orgaeff ~ condition, data = main2_sub_numfact)
summary(orgaeff_cond_rob) # no significant or valence changes: cond1: b = -1.32, p < .001; cond2: b = -.13, p = .418
confint(orgaeff_cond_rob) # cond1: CI[-1.57; -1.07]; cond2: CI[-.45; .19]

# regression 2 (legit ~ condition + orgaeff)

legit_cond_orgaeff <- lm(legit ~ condition + orgaeff, data = main2_sub_numfact)
summary(legit_cond_orgaeff)

legit_cond_orgaeff_cooksd <- cooks.distance(legit_cond_orgaeff)

plot(legit_cond_orgaeff_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(legit_cond_orgaeff_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(legit_cond_orgaeff_cooksd)+1, y=legit_cond_orgaeff_cooksd, labels=ifelse(legit_cond_orgaeff_cooksd>4*mean(legit_cond_orgaeff_cooksd, na.rm=T),names(legit_cond_orgaeff_cooksd),""), col="red")  # add labels

influential <- as.numeric(names(legit_cond_orgaeff_cooksd)[legit_cond_orgaeff_cooksd > 4*mean(legit_cond_orgaeff_cooksd, na.rm=T)])  # influential row numbers
head(main2_sub_numfact[influential, ], n = 30)  # identifying outliers 

# => 25 outliers: ID 5, 20, 24, 28, 47, 50, 91, 103, 141, 144, 165,170, 201, 205, 206, 269, 277, 294, 299, 306, 330,  378, 382, 387, 389 

car::outlierTest(legit_cond_orgaeff) # 165 most extreme outlier (man, 56y, condtion: exp1, right, low orgaeff but high legit) 

# run again without outliers 

noutliers2 <- c("5", "20", "24", "28", "47", "50", "91", "103", "141", "144", "165", "170", "201", "205", "206", "269", "277", "294", "299", "306", "330", "378", "382", "387", "389")

main2_sub_numfact_noutliers2 <- main2_sub_numfact %>%
  filter(!id %in% noutliers2)

legit_cond_orgaeff_nout <- lm(legit ~ condition + orgaeff, data = main2_sub_numfact_noutliers2)
summary(legit_cond_orgaeff_nout) # no significant or valence changes: cond1: b = .67, p < .001; cond2: b = .42, p < .001; orgaeff: b = .63, p < .001

# robust regression 2

legit_cond_orgaeff_rob <- lmrob(legit ~ condition + orgaeff, data = main2_sub_numfact)
summary(legit_cond_orgaeff_rob) # no significant or valence changes: cond1: b = .63, p < .001; cond2: b = .35, p = .011; orgaeff: b = .60, p < .001
confint(legit_cond_orgaeff_rob) # cond1: CI[.36; .91]; cond2: CI[.08; .62]; orgaeff: CI[.50; .71]

# regression 3 (support ~ condition + orgaeff + legit)

support_cond_orgaeff_legit <- lm(support ~ condition + orgaeff + legit, data = main2_sub_numfact)
summary(support_cond_orgaeff_legit)

support_cond_orgaeff_legit_cooksd <- cooks.distance(support_cond_orgaeff_legit)

plot(support_cond_orgaeff_legit_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(support_cond_orgaeff_legit_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(support_cond_orgaeff_legit_cooksd)+1, y=support_cond_orgaeff_legit_cooksd, labels=ifelse(support_cond_orgaeff_legit_cooksd>4*mean(support_cond_orgaeff_legit_cooksd, na.rm=T),names(support_cond_orgaeff_legit_cooksd),""), col="red")  # add labels

influential <- as.numeric(names(support_cond_orgaeff_legit_cooksd)[support_cond_orgaeff_legit_cooksd > 4*mean(support_cond_orgaeff_legit_cooksd, na.rm=T)])  # influential row numbers
head(main2_sub_numfact[influential, ], n = 30)  # identifying outliers 

# => 17 outliers: ID 30, 35, 58, 71, 101, 108, 154, 165, 184, 237, 300, 304, 346, 358, 374, 376, 395 

car::outlierTest(support_cond_orgaeff_legit) # 184 most extreme outlier (man, 45y, condtion: control, centre, high scores throughout but only 1 on support)

# run again without outliers 

noutliers3 <- c("30", "35", "58", "71", "101", "108", "154", "165", "184", "237", "300", "304", "346", "358", "374", "376", "395")

main2_sub_numfact_noutliers3 <- main2_sub_numfact %>%
  filter(!id %in% noutliers3)

support_cond_orgaeff_legit_nout <- lm(support ~ condition + orgaeff + legit, data = main2_sub_numfact_noutliers3)
summary(support_cond_orgaeff_legit_nout) # condition2 turns negative but stays n.s.: cond1: b = .43, p = .00; cond2: b = -.03, p = .80; orgaeff: b = .17, p = .00; legit: b = .85, p < .001

# robust regression 3

support_cond_orgaeff_legit_rob <- lmrob(support ~ condition + orgaeff + legit, data = main2_sub_numfact)
summary(support_cond_orgaeff_legit_rob) # condition2 turns negative but stays n.s.: cond1: b = .45, p = .003; cond2: b = -.02, p = .873; orgaeff: b = .15, p = .017; legit: b = .85, p < .001
confint(support_cond_orgaeff_legit_rob) # cond1: CI[.16; .75]; cond2: CI[-.31; .26]; orgaeff: CI[.03;.27]; legit: CI[.74; .96]

# overall 51 outliers (5 overlapp across vectors)
intersect(noutliers1, noutliers2) # 294
intersect(noutliers1, noutliers3) # 58, 71, 184
intersect(noutliers2, noutliers3) # 165

## moderation analysis (center = 1, 2: mean-centering all mediators and moderator)----

#OLD mod_orgaeff <- process (data=main2_sub_numcond,y="support",x="condition",m= c("orgaeff", "legit"),w="selfcat",modelbt = 1, mcx = 3, center = 1,model=89, jn = 1, boot = 10000, plot=1, seed=09922)
#NEW
process(data = main2_sub_numcond, y = "support", x = "condition", m = c("orgaeff", "legit"), w = "selfcat", modelbt = 1, mcx = 3, center = 2, model = 89, jn = 1, boot = 10000, plot = 1, moments = 1, seed = 311022)
process(data = main2_sub_numcond, y = "support", x = "condition", m = c("orgaeff", "legit"), w = "selfcat", modelbt = 1, mcx = 3, model = 89, jn = 1, boot = 10000, plot = 1, moments = 1, seed = 311022)

# visualiation

# creating dataset for interaction plot legit, support, selfcat

legit_int <- c(-1.236, 0.0000, 1.3638, -1.236, 0.0000, 1.3638, -1.236, 0.0000, 1.3638)
selfcat_int <- c(-1.6714, -1.6714, -1.6714, 0.0000, 0.0000, 0.0000, 1.6714,1.6714, 1.6714)
support_int <- c(1.2840, 1.4957, 1.7074, 2.0042, 2.4923, 2.9803, 2.7244, 3.4888, 4.2532)

df <- data.frame(legit_int, selfcat_int, support_int)

df$selfcat_int <- factor(x = df$selfcat_int, labels = c("-1SD", "Mean", "+1SD"))
df$legit_int <- factor(x = df$legit_int)

interaction.plot(x.factor = df$legit_int, 
                 trace.factor = df$selfcat_int,
                 response = df$support_int,
                 fun = median,
                 legend = T,
                 ylab = "Support intention",
                 xlab = "Perceived legitimacy",
                 trace.label = "Social identification",
                 col = c("blue", "red", "green"),
                 lyt = 1,
                 lwd = 3
)


# visualisation (jn output)

cselfcat_jn <- c(-2.4802,-2.1802, -1.9492,-1.8802,-1.5802,1.2802,-0.9802,-0.6802,-0.3802,-0.0802,0.2198, 
                0.5198, 0.8198,1.1198,1.4198, 1.7198, 2.0198,2.3198,2.6198,2.9198,3.2198,3.5198)  
selfcat_jn <- c(1, 1.3, 1.5310, 1.6, 1.9, 2.2, 2.5, 2.8, 3.1, 3.4, 3.7, 4, 4.3, 4.6, 4.9, 5.2, 5.5, 5.8, 6.1, 6.4, 6.7, 7)
effect_jn <- c(0.0572, 0.0936, 0.1216, 0.1299 , 0.1663, 0.2027, 0.2391, 0.2754, 0.3118, 0.3482, 0.3845, 
               0.4209, 0.4573, 0.4937, 0.5300, 0.5664, 0.6028, 0.6391, 0.6755, 0.7119, 0.7483, 0.7846)
llci_jn <- c(-0.0794,-0.0343,0.0000, 0.0101,0.0536,0.0960, 0.1371,0.1768, 0.2148, 0.2510, 0.2856,0.3186,0.3501, 0.3804,0.4096, 0.4378, 0.4653,0.4923,0.5187, 0.5447, 0.5704, 0.5958)                                                                                                
ulci_jn <- c(0.1938, 0.2214, 0.2431, 0.2497, 0.2790, 0.3093, 0.3410, 0.3741, 0.4088, 0.4453,0.4834, 0.5232, 0.5644, 0.6069, 0.6505, 0.6950, 0.7402, 0.7860, 0.8323, 0.8791, 0.9261, 0.9735)

plot(x=selfcat_jn,y=effect_jn,type="l",pch=19,ylim=c(0, 1),xlim=c(1,7),lwd=3,
     ylab="Conditional effect of legitimacy on support intention",
     xlab="Social identification(W)",col="red")
points(selfcat_jn,llci_jn,lwd=2,lty=2,type="l",col="black")
points(selfcat_jn,ulci_jn,lwd=2,lty=2,type="l",col="black")
abline(h=0,untf = FALSE,lty=3,lwd=1,col="red")
abline(v=1.3,untf=FALSE,lty=3,lwd=1)
text(1.3,"1.3",cex=0.8)

# creating dataset for interaction plot selfcat, support, condition
condition_int <- c(1.0000, 2.000, 3.0000, 1.0000, 2.0000, 3.0000, 1.0000, 2.0000, 3.0000)
selfcat_int <- c(1.8088, 1.8088, 1.8088, 3.4802, 3.4802, 3.4802, 5.1515, 5.1515, 5.1515)
support_int2 <- c(1.4863, 1.4847, 1.5162, 2.3138, 2.6055, 2.5554, 3.1413, 3.7264, 3.5946)

df2 <- data.frame(condition_int, selfcat_int, support_int2)
df2$selfcat_int <- factor(x = df2$selfcat_int, labels = c("-1SD", "Mean", "+1SD"))
df2$condition_int <- factor(x = df2$condition_int, labels = c("Control", "Experimental 1 (no mockery)", "Experimental 2 (mockery"))


interaction.plot(x.factor = df2$selfcat_int, 
                 trace.factor = df2$condition_int,
                 response = df2$support_int2,
                 fun = median,
                 legend = T,
                 ylab = "Support intention",
                 xlab = "Social identification",
                 trace.label = "Experimental condition",
                 col = c("blue", "red", "green"),
                 lyt = 1,
                 lwd = 3
)


# manual centring orgaeff, stereo, legit and selfcat
main2_sub_numfact <- main2_sub_numfact %>%
  mutate(cselfcat = scale(selfcat, scale = FALSE),
         corgaeff = scale(orgaeff, scale = FALSE),
         clegit = scale(legit, scale = FALSE),
         cstereo = scale(stereo, scale= FALSE))

# regression 3 (support ~ condition + corgaeff + clegit + cselfcat + condition*cselfcat + corgaeff*cselfcat + clegit*cselfcat; 
# regressions 1 and 2, see above)
support_cond_orgaeff_legit_selfcat <- lm(support ~ condition + corgaeff + clegit + cselfcat + condition*cselfcat + corgaeff*cselfcat + clegit*cselfcat, data = main2_sub_numfact)
summary(support_cond_orgaeff_legit_selfcat)

# visualisation other interactions
library(sjPlot)
library(sjmisc)
library(ggplot2)

# run regression 3 again without centring (does not work with plot_model function)
support_cond_orgaeff_legit_selfcat2 <- lm(support ~ condition + orgaeff + legit + selfcat + condition*selfcat + orgaeff*selfcat + legit*selfcat, data = main2_sub_numfact)
plot_model(support_cond_orgaeff_legit_selfcat2, type = "pred", terms = c("legit", "selfcat"), axis.title = c("Perceived legitmacy","Support intention"), legend.title = "Levels of social identificaiton", title = "")
plot_model(support_cond_orgaeff_legit_selfcat2, type = "pred", terms = c("selfcat", "condition"), axis.title = c("Social identificaiton","Support intention"), legend.title = "Experimental conditions", title = "")

# plot outliers
support_cond_orgaeff_legit_selfcat_cooksd <- cooks.distance(support_cond_orgaeff_legit_selfcat)

plot(support_cond_orgaeff_legit_selfcat_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(support_cond_orgaeff_legit_selfcat_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(support_cond_orgaeff_legit_selfcat_cooksd)+1, y=support_cond_orgaeff_legit_selfcat_cooksd, labels=ifelse(support_cond_orgaeff_legit_selfcat_cooksd>4*mean(support_cond_orgaeff_legit_selfcat_cooksd, na.rm=T),names(support_cond_orgaeff_legit_selfcat_cooksd),""), col="red")  # add labels

influential <- as.numeric(names(support_cond_orgaeff_legit_selfcat_cooksd)[support_cond_orgaeff_legit_selfcat_cooksd > 4*mean(support_cond_orgaeff_legit_selfcat_cooksd, na.rm=T)])  # influential row numbers
head(main2_sub_numfact[influential, ], n = 30)  # identifying outliers 

# 13 outliers: ID 6, 22, 71, 154, 184, 189, 237, 263, 322, 328, 371, 418, 428

car::outlierTest(support_cond_orgaeff_legit_selfcat) # most extreme outliers: ID 184 (control cond, man, 45y, centre, despite high scores, only support of 1) and 237 (control cond, woman, 61y, right, despite moderate to high scores, only support of 2)

# run again without outliers 

noutliers4 <- c("6", "22", "71", "154", "184", "189", "237", "263", "322", "328", "371", "418", "428")

main2_sub_numfact_noutliers4 <- main2_sub_numfact %>%
  filter(!id %in% noutliers4)

support_cond_orgaeff_legit_selfcat_nout <- lm(support ~ condition + corgaeff + clegit + cselfcat + condition*cselfcat + corgaeff*cselfcat + clegit*cselfcat, data = main2_sub_numfact_noutliers4)
summary(support_cond_orgaeff_legit_selfcat_nout) # condition 1 turns marginal sign., interaction condition1*selfcat now marginal sign.

# robust regression 3

support_cond_orgaeff_legit_selfcat_rob <- lmrob(support ~ condition + corgaeff + clegit + cselfcat + condition*cselfcat + corgaeff*cselfcat + clegit*cselfcat,data = main2_sub_numfact)
summary(support_cond_orgaeff_legit_selfcat_rob) # condition1 turns marginal sign., interaction condition1*selfcat now n.s.
# condition1: b = .21, p = .061; condition2: b = -.10, p = .339; corgaeff: b = .06, p = .172; clegit: b = .42, p < .001; cselfcat: b = .61, p < .001
# condition1*cselfcat: b = .09, p = .086; condition2*selfcat: b = -.04, p = .460; corgaeff*cselfcat: b = .01, p = .500; 
# clegit*cselfcat: b = .16, p < .001
confint(support_cond_orgaeff_legit_selfcat_rob) # condition1: CI [-.01; .44]; condition2: CI [-.30; .10]; corgaeff: CI [-.02; .14];
# clegit: CI [.33; .51]; condition1*cselfcat: CI [-.01; .20]; condition2*cselfcat: CI [-.14; .06]; corgaeff*cselfcat: CI [-.03; .05];
# clegit*cselfcat: CI [.13; .20]

# overall 45 outliers (7 overlapp across vectors)
intersect(noutliers1, noutliers2) # 294
intersect(noutliers1, noutliers4) # 6, 71, 184, 328, 371, 428
intersect(noutliers2, noutliers4) # none 

# inspection whether found outliers that affected regression (and that cause robust regression to dimish the sign. effect of condition1 
# and the interaction with self-cat) overlap with the multivariate ones driving support intention based on perceived legitimacy and self-cat

intersect(outliers_legit_support, noutliers4) # eight (out of 13) outliers overlap: IDs "22"  "71"  "154" "184" "263" "322" "371" "428"

# stereo = M1

med_stereo <- process(data = main2_sub_numcond, y = "support", x = "condition", m = c("stereo", "legit"), modelbt =1, mcx = 3, total = 1, model = 6, boot = 10000, seed = 12922) 

#...





## not pre-registred analyses----

# moderation of condition --> legitimacy, W = selfcat
mod_orgaeff2 <- process (data=main2_sub_numcond,y="legit",x="condition",w="selfcat",modelbt = 1, mcx = 3, center = 1, model=1, boot = 10000, plot=1, seed=13922)


# moderation of condition --> legitimacy, W = orgaeff
mod_orgaeff3 <- process (data=main2_sub_numcond,y="legit",x="condition",w="orgaeff",modelbt = 1, mcx = 3, center = 1,model=1, boot = 10000, plot=1, seed=13922)
mod_orgaeff3 <- process (data=main2_sub_numcond,y="support",x="condition",m = "legit", modelbt = 1, mcx = 3, center = 1,model=4, boot = 10000, plot=1, seed=13922)





