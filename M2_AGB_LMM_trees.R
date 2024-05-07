# Script: M2_AGB_LMM_trees

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

### Read in data ### 
source("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/biomass/agb_calculations/M1_AGB_calculations.R")
glimpse(agb.growth.trees)
glimpse(tree.data.final)

### Change data types ###
tree.data.final$treatment.perc.cut <- factor(tree.data.final$treatment.perc.cut, levels = c(0, 60, 80, 100), ordered = TRUE)
tree.data.final$coi.23 <- factor(tree.data.final$coi.23, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Load.18 <- factor(tree.data.final$Load.18, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Load.19 <- factor(tree.data.final$Load.19, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$Site <- as.factor(tree.data.final$Site)
tree.data.final$Block <- as.factor(tree.data.final$Block)
tree.data.final$Plot <- as.factor(tree.data.final$Plot)

### Merge t.d.f and agb ###
# firstly I'll just keep the cols I want from t.d.f
variables_list <- c("unique.ID", "Site", "Block", "TPL.name", "Cut", "treatment.perc.cut", "Diam.18", "Diam.19",  "pre_liana_presence", "post_liana_presence")
tree.data.final <- tree.data.final %>% dplyr::select(all_of(variables_list))

# merge
agb.full <- merge(agb.growth.trees, tree.data.final, by = "unique.ID")
colnames(agb.full)
agb.full <- agb.full[ ,c("unique.ID", "Site", "Block", "Plot", "TPL.name", "Cut", "treatment.perc.cut", "Diam.18", "Diam.19", "pre_liana_presence", 
                         "post_liana_presence", "agb.growth.18", "agb.growth.19")] # reorder cols 

### Create dataset where there are only values (no NAs) for all variables going in the models ###
# need to do this to remove NAs and make sure db length is the same for transformations, models etc.
db.19 <- agb.full[which(complete.cases(agb.full[,c("Site", "Block", "Plot", "TPL.name", "Cut", "treatment.perc.cut", 
                                                   "Diam.19", "agb.growth.19", "pre_liana_presence", "post_liana_presence")])),]
db.18 <- agb.full[which(complete.cases(agb.full[,c("Site", "Block", "Plot", "TPL.name", "Cut", "treatment.perc.cut", 
                                                   "Diam.18", "agb.growth.18", "pre_liana_presence", "post_liana_presence")])),]

### TRANSFORMATION ### ----
hist(db.19$agb.growth.19) # fine
hist(db.18$agb.growth.18) # fine

### MODELS ### ----
## AGB 
# response variable: agb (kg/month/tree)
# all fixed effects: treatment (integer), original diameter (dbl), pre and post liana presence. 
# random effects: site, block, plot (in that order), species

# 1. 2019-2023 ----
# full model
agb19.m1 <- lmer(agb.growth.19 ~ treatment.perc.cut + Diam.19 + pre_liana_presence + post_liana_presence + 
                   (1|Site/Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m1)
vif(agb19.m1) # check multicollinearity is below 10; it's fine

# reduced models
agb19.m2 <- lmer(agb.growth.19 ~ Diam.19 + pre_liana_presence + post_liana_presence + 
                   (1|Site/Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m2)

agb19.m3 <- lmer(agb.growth.19 ~ treatment.perc.cut + pre_liana_presence + post_liana_presence + 
                   (1|Site/Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m3)

agb19.m4 <- lmer(agb.growth.19 ~ treatment.perc.cut + Diam.19 + post_liana_presence + 
                   (1|Site/Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m4)

agb19.m5 <- lmer(agb.growth.19 ~ treatment.perc.cut + Diam.19 + pre_liana_presence + 
                   (1|Site/Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m5)

agb19.m6 <- lmer(agb.growth.19 ~ treatment.perc.cut + Diam.19 + pre_liana_presence + post_liana_presence + 
                   (1|TPL.name), 
                 na.action = na.omit, data = db.19); summary(agb19.m6)

agb19.m7 <- lmer(agb.growth.19 ~ treatment.perc.cut + Diam.19 + pre_liana_presence + post_liana_presence + 
                   (1|Site/Block/Plot), 
                 na.action = na.omit, data = db.19); summary(agb19.m7)

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/biomass/agb_results/likelihood_ratio_test_results/full_treatment/all_spp/agb19/")
anova(agb19.m1,agb19.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(agb19.m1,agb19.m3) %>% as.data.frame() %>% write.csv(file = "originaldiam_results.csv") # testing original diam
anova(agb19.m1,agb19.m4) %>% as.data.frame() %>% write.csv(file = "pre_liana_pres_results.csv") # testing pre_liana_presence
anova(agb19.m1,agb19.m5) %>% as.data.frame() %>% write.csv(file = "post_liana_pres_results.csv") # testing post_liana_presence
anova(agb19.m1,agb19.m6) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(agb19.m1,agb19.m7) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing spp.

# 1. 2018-2023 ----
# full model
agb18.m1 <- lmer(agb.growth.18 ~ treatment.perc.cut + Diam.18 + pre_liana_presence + post_liana_presence + 
                   (1|Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m1)
vif(agb18.m1) # check multicollinearity is below 10; it's fine

# reduced models
agb18.m2 <- lmer(agb.growth.18 ~ Diam.18 + pre_liana_presence + post_liana_presence + 
                   (1|Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m2)

agb18.m3 <- lmer(agb.growth.18 ~ treatment.perc.cut + pre_liana_presence + post_liana_presence + 
                   (1|Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m3)

agb18.m4 <- lmer(agb.growth.18 ~ treatment.perc.cut + Diam.18 + post_liana_presence + 
                   (1|Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m4)

agb18.m5 <- lmer(agb.growth.18 ~ treatment.perc.cut + Diam.18 + pre_liana_presence + 
                   (1|Block/Plot) + (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m5)

agb18.m6 <- lmer(agb.growth.18 ~ treatment.perc.cut + Diam.18 + pre_liana_presence + post_liana_presence + 
                   (1|TPL.name), 
                 na.action = na.omit, data = db.18); summary(agb18.m6)

agb18.m7 <- lmer(agb.growth.18 ~ treatment.perc.cut + Diam.18 + pre_liana_presence + post_liana_presence + 
                   (1|Block/Plot), 
                 na.action = na.omit, data = db.18); summary(agb18.m7)

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/biomass/agb_results/likelihood_ratio_test_results/full_treatment/all_spp/agb18/")
anova(agb18.m1,agb18.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment
anova(agb18.m1,agb18.m3) %>% as.data.frame() %>% write.csv(file = "originaldiam_results.csv") # testing original diam
anova(agb18.m1,agb18.m4) %>% as.data.frame() %>% write.csv(file = "pre_liana_pres_results.csv") # testing pre_liana_presence
anova(agb18.m1,agb18.m5) %>% as.data.frame() %>% write.csv(file = "post_liana_pres_results.csv") # testing post_liana_presence
anova(agb18.m1,agb18.m6) %>% as.data.frame() %>% write.csv(file = "siteblockplot_results.csv") # testing random spatial setup
anova(agb18.m1,agb18.m7) %>% as.data.frame() %>% write.csv(file = "species_results.csv") # testing spp.

