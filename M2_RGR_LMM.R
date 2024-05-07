# Script: M2_RGR_LMM

### SET UP ### ----

### Read in packages ### 
library(lme4)
library(dplyr)
library(Matrix)
library(ggplot2)
library(arm)
library(broom.mixed)
library(timeplyr)
library(lubridate)
library(bestNormalize)
library(car)

### Run M1_RGR_calculations to read in data ### 
source("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/rgr/RGR_calculations/M1_RGR_calculations.R")
glimpse(tree.data.final)

### Change data types ###
tree.data.final$treatment.perc.cut <- factor(tree.data.final$treatment.perc.cut, levels = c(0, 60, 80, 100), ordered = TRUE)
tree.data.final$coi.23 <- factor(tree.data.final$coi.23, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Load.18 <- factor(tree.data.final$Load.18, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Load.19 <- factor(tree.data.final$Load.19, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Site <- as.factor(tree.data.final$Site)
tree.data.final$Block <- as.factor(tree.data.final$Block)
tree.data.final$Plot <- as.factor(tree.data.final$Plot)
tree.data.final$Dead.19 <- as.numeric(tree.data.final$Dead.19)

### Let's just take what we need ###
colnames(tree.data.final)
data_all <- tree.data.final[-which(tree.data.final$remove_rgr == "1"), ] # remove the rows of data I have decided to exclude in the rgr models. reasons can be found in the why_remove column.
#data_all_2 <- data_all_1[-which(data_all_1$timber_spp == "0"), ] # remove non-timbers

### Separate age classes ###
# at this point, we are removing saplings and seedlings
adult.db <- subset(data_all, Age.class == "adult")
juv.db <- subset(data_all, Age.class == "juvenile")

### Create dataset where there are only values (no NAs) for all variables going in the models ###
# need to do this to remove NAs and make sure db length is the same for transformations, models etc.
adult.db.19 <- adult.db[which(complete.cases(adult.db[,c("rgr.19", "Cut","treatment.perc.cut", "Diam.19", "Load.19",
                                                           "total.stems.23", "Site", "Block", "Plot", "TPL.name", "coi.23")])),]
adult.db.18 <- adult.db[which(complete.cases(adult.db[,c("rgr.18", "Cut", "treatment.perc.cut", "Diam.18", "Load.18",
                                                         "total.stems.23", "Block", "Plot", "TPL.name", "coi.23")])),]
juv.db.19 <- juv.db[which(complete.cases(juv.db[,c("rgr.19", "Cut", "treatment.perc.cut", "Diam.19", "Prev.19", "prev.23",
                                                         "Site", "Block", "Plot", "TPL.name")])),]
juv.db.18 <- juv.db[which(complete.cases(juv.db[,c("rgr.18", "Cut", "treatment.perc.cut", "Diam.18", "Prev.18", "prev.23",
                                                         "Block", "Plot", "TPL.name")])),]

### TRANSFORMATION ### ----
hist(adult.db.19$rgr.19) # normal
hist(adult.db.18$rgr.18) # normal
hist(juv.db.19$rgr.19) # normal
hist(juv.db.18$rgr.18) # normal
# no need for transformations

## Let's get bestNormalize to pick the best transformation for each rgr response variable
# adult 2019
# bN_adult_19 <- bestNormalize(adult.db.19$rgr.19); bN_adult_19 # orderNorm
# MASS::truehist(bN_adult_19$x.t, main = "orderNorm transformation", nbins = 12) # because bN chose orderNorm, x.t will be the oN transformation
# adult.db.19$rgr.19.xt <- bN_adult_19$x.t
# 
# # adult 2018
# bN_adult_18 <- bestNormalize(adult.db.18$rgr.18); bN_adult_18 # orderNorm
# MASS::truehist(bN_adult_18$x.t, main = "orderNorm transformation", nbins = 12) # 
# adult.db.18$rgr.18.xt <- bN_adult_18$x.t
# 
# # juvenile 2019 
# bN_juv_19 <- bestNormalize(juv.db.19$rgr.19); bN_juv_19 # orderNorm
# MASS::truehist(bN_juv_19$x.t, main = "orderNorm transformation", nbins = 12) # 
# juv.db.19$rgr.19.xt <- bN_juv_19$x.t
# 
# # juvenile 2018
# bN_juv_18 <- bestNormalize(juv.db.18$rgr.18); bN_juv_18 # orderNorm
# MASS::truehist(bN_juv_18$x.t, main = "orderNorm transformation", nbins = 12) # 
# juv.db.18$rgr.18.xt <- bN_juv_18$x.t



### MODELS ### ----
## RGR 
# response variable: monthly rgr (adult or juv)
# all fixed effects: treatment (integer), original diameter (dbl). 
# juv fixed effects: original and 2023 liana prevalence on tree (binary). 
# adult fixed effects: original and 2023 COI/load (integer), total stems 23 (integer). not include canopy liana as Will said this is rubbish.
# random effects: site, block, plot (in that order), species

# 1. Adults 2019-2023 ----
# full model
rgr19.adult.m1 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Load.19 + coi.23 + total.stems.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                         na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m1)
vif(rgr19.adult.m1) # check multicollinearity is below 10; it's fine

# reduced models
rgr19.adult.m2 <- lmer(rgr.19 ~ 
                         Diam.19 + Load.19 + coi.23 + total.stems.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                         na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m2) # treatment removed

rgr19.adult.m3 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Load.19 + coi.23 + total.stems.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                         na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m3) # diam.19 removed

rgr19.adult.m4 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + coi.23 + total.stems.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                         na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m4) # load.19 removed

rgr19.adult.m5 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Load.19 + total.stems.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m5) # coi.23 removed

rgr19.adult.m6 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Load.19 + coi.23 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m6) # total.stems.23 removed

rgr19.adult.m7 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Load.19 + coi.23 + total.stems.23 + 
                         (1|TPL.name), 
                       na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m7) # random site/block/plot removed

rgr19.adult.m8 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Load.19 + coi.23 + total.stems.23 + 
                         (1|Site/Block/Plot), 
                       na.action = na.omit, data = adult.db.19); summary(rgr19.adult.m8) # species removed

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/rgr/rgr_results/likelihood_ratio_test_results/full_treatment/all_spp/rgr19_adult/")
anova(rgr19.adult.m1,rgr19.adult.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(rgr19.adult.m1,rgr19.adult.m3) %>% as.data.frame() %>% write.csv(file = "pre_diam_results.csv") # testing diam.19
anova(rgr19.adult.m1,rgr19.adult.m4) %>% as.data.frame() %>% write.csv(file = "pre_coi_results.csv") # testing load.19
anova(rgr19.adult.m1,rgr19.adult.m5) %>% as.data.frame() %>% write.csv(file = "post_coi_results.csv") # testing coi.23
anova(rgr19.adult.m1,rgr19.adult.m6) %>% as.data.frame() %>% write.csv(file = "post_stemsliana_results.csv") # testing total.stems.23
anova(rgr19.adult.m1,rgr19.adult.m7) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(rgr19.adult.m1,rgr19.adult.m8) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing spp.



# 2. Adults 2018-2023 ----
# full model
rgr18.adult.m1 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + Load.18 + coi.23 + total.stems.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m1)
vif(rgr18.adult.m1) # check multicollinearity is below 10; it's fine

# reduced models
rgr18.adult.m2 <- lmer(rgr.18 ~ 
                         Diam.18 + Load.18 + coi.23 + total.stems.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m2) # treatment removed

rgr18.adult.m3 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Load.18 + coi.23 + total.stems.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m3) # diam.18 removed

rgr18.adult.m4 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + coi.23 + canopy.liana.23 + total.stems.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m4) # load.18 removed

rgr18.adult.m5 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + Load.18 + total.stems.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m5) # coi.23 removed

rgr18.adult.m6 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + Load.18 + coi.23 + 
                         (1|Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m6) # total.stems.23 removed

rgr18.adult.m7 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + Load.18 + coi.23 + total.stems.23 + 
                         (1|TPL.name), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m7) # random site/block/plot removed

rgr18.adult.m8 <- lmer(rgr.18 ~ 
                         treatment.perc.cut + Diam.18 + Load.18 + coi.23 + total.stems.23 + 
                         (1|Block/Plot), 
                       na.action = na.omit, data = adult.db.18); summary(rgr18.adult.m8) # species removed

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/rgr/rgr_results/likelihood_ratio_test_results/full_treatment/all_spp/rgr18_adult/")
anova(rgr18.adult.m1,rgr18.adult.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(rgr18.adult.m1,rgr18.adult.m3) %>% as.data.frame() %>% write.csv(file = "pre_diam_results.csv") # testing diam.18
anova(rgr18.adult.m1,rgr18.adult.m4) %>% as.data.frame() %>% write.csv(file = "pre_coi_results.csv") # testing load.18
anova(rgr18.adult.m1,rgr18.adult.m5) %>% as.data.frame() %>% write.csv(file = "post_coi_results.csv") # testing coi.23
anova(rgr18.adult.m1,rgr18.adult.m6) %>% as.data.frame() %>% write.csv(file = "post_stemsliana_results.csv") # testing total.stems.23
anova(rgr18.adult.m1,rgr18.adult.m7) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(rgr18.adult.m1,rgr18.adult.m8) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing spp.


# all fixed effects: treatment (integer), original diameter (dbl). 
# juv fixed effects: original and 2023 liana prevalence on tree (binary). 
# adult fixed effects: original and 2023 COI/load (integer), canopy liana (integer), total stems 23 (integer).
# random effects: site, block, plot (in that order), species

# 3. Juvenile 2019-2023 ----
# full model
rgr19.juv.m1 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Prev.19 + prev.23 +
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m1)
vif(rgr19.juv.m1) # check multicollinearity is below 10; it's fine

# reduced models
rgr19.juv.m2 <- lmer(rgr.19 ~ 
                         Diam.19 + Prev.19 + prev.23 +
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m2) # treatment removed

rgr19.juv.m3 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Prev.19 + prev.23 +
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m3) # diam.19 removed

rgr19.juv.m4 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + prev.23 +
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m4) # prev.19 removed

rgr19.juv.m5 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Prev.19 + 
                         (1|Site/Block/Plot) + (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m5) # prev.23 removed

rgr19.juv.m6 <- lmer(rgr.19 ~ 
                         treatment.perc.cut + Diam.19 + Prev.19 + prev.23 +
                         (1|TPL.name), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m6) # random site/block/plot removed

rgr19.juv.m7 <- lmer(rgr.19 ~ 
                       treatment.perc.cut + Diam.19 + Prev.19 + prev.23 +
                       (1|Site/Block/Plot), 
                       na.action = na.omit, data = juv.db.19); summary(rgr19.juv.m7) # species removed

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/rgr/rgr_results/likelihood_ratio_test_results/full_treatment/all_spp/rgr19_juv/")
anova(rgr19.juv.m1,rgr19.juv.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(rgr19.juv.m1,rgr19.juv.m3) %>% as.data.frame() %>% write.csv(file = "pre_diam_results.csv") # testing diam.19
anova(rgr19.juv.m1,rgr19.juv.m4) %>% as.data.frame() %>% write.csv(file = "pre_liana_prev_results.csv") # testing prev.19
anova(rgr19.juv.m1,rgr19.juv.m5) %>% as.data.frame() %>% write.csv(file = "post_liana_prev_results.csv") # testing prev.23
anova(rgr19.juv.m1,rgr19.juv.m6) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(rgr19.juv.m1,rgr19.juv.m7) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing random spp.



# 4. Juvenile 2018-2023 ----
# full model
rgr18.juv.m1 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Diam.18 + Prev.18 + prev.23 +
                       (1|Block/Plot) + (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m1)
vif(rgr18.juv.m1) # check multicollinearity is below 10; it's fine

# reduced models
rgr18.juv.m2 <- lmer(rgr.18 ~ 
                       Diam.18 + Prev.18 + prev.23 +
                       (1|Block/Plot) + (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m2) # treatment removed

rgr18.juv.m3 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Prev.18 + prev.23 +
                       (1|Block/Plot) + (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m3) # diam.18 removed

rgr18.juv.m4 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Diam.18 + prev.23 +
                       (1|Block/Plot) + (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m4) # prev.18 removed

rgr18.juv.m5 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Diam.18 + Prev.18 + 
                       (1|Block/Plot) + (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m5) # prev.23 removed

rgr18.juv.m6 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Diam.18 + Prev.18 + prev.23 +
                       (1|TPL.name), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m6) # random site/block/plot removed

rgr18.juv.m7 <- lmer(rgr.18 ~ 
                       treatment.perc.cut + Diam.18 + Prev.18 + prev.23 +
                       (1|Block/Plot), 
                     na.action = na.omit, data = juv.db.18); summary(rgr18.juv.m7) # species removed

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/rgr/rgr_results/likelihood_ratio_test_results/full_treatment/all_spp/rgr18_juv/")
anova(rgr18.juv.m1,rgr18.juv.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(rgr18.juv.m1,rgr18.juv.m3) %>% as.data.frame() %>% write.csv(file = "pre_diam_results.csv") # testing diam.18
anova(rgr18.juv.m1,rgr18.juv.m4) %>% as.data.frame() %>% write.csv(file = "pre_liana_prev_results.csv") # testing prev.18
anova(rgr18.juv.m1,rgr18.juv.m5) %>% as.data.frame() %>% write.csv(file = "post_liana_prev_results.csv") # testing prev.23
anova(rgr18.juv.m1,rgr18.juv.m6) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(rgr18.juv.m1,rgr18.juv.m7) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing random spp.

