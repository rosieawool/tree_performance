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
glimpse(agb.growth.plot)
glimpse(tree.data.final)

### Change data types ###
tree.data.final$Site <- as.factor(tree.data.final$Site)
tree.data.final$Block <- as.factor(tree.data.final$Block)
tree.data.final$Plot <- as.factor(tree.data.final$Plot)

### We need to aggregate t.d.f to plot level ###
# firstly I'll just keep the cols I want from t.d.f to make it clearer
variables_list <- c("unique.ID", "Site", "Block", "Plot", "TPL.name", "Cut", "treatment.perc.cut", "pre_liana_presence", "post_liana_presence")
tree.data.final <- tree.data.final %>% dplyr::select(all_of(variables_list))

# now let's aggregate treatment to plot level
tdf.plot <-
  stats::aggregate(
    tree.data.final[, 6:7], # the [, 3:4] is needed to remove the unique.ID & Plot cols from the sum calculations
    by = list(tree.data.final$Plot),
    na.rm = T,
    FUN = mean
  )
colnames(tdf.plot)
tdf.plot <- tdf.plot %>% dplyr::rename(Plot = Group.1)
# change data type here as does not work if done before aggregation
tdf.plot$treatment.perc.cut <- factor(tdf.plot$treatment.perc.cut, levels = c(0, 60, 80, 100), ordered = TRUE)

### Merge t.d.f and agb ###
# merge
agb.full <- merge(agb.growth.plot, tdf.plot, by = "Plot") # left join

### Create dataset where there are only values (no NAs) for all variables going in the models ###
# need to do this to remove NAs and make sure db length is the same for transformations, models etc.
db.19 <- agb.full[which(complete.cases(agb.full[,c("Plot", "Cut", "treatment.perc.cut", "agb.growth.19")])),]
db.18 <- agb.full[which(complete.cases(agb.full[,c("Plot", "Cut", "treatment.perc.cut", "agb.growth.18")])),]

### TRANSFORMATION ### ----
hist(db.19$agb.growth.19) # fine?
hist(db.18$agb.growth.18) # fine?

### MODELS ### ----
## AGB 
# response variable: agb (kg/month/plot)
# all fixed effects: treatment (integer), original diam (dbl)
# random effects: site, block, plot (in that order), species

# 1. 2019-2023 ----
# full model
agb19.m1 <- lm(agb.growth.19 ~ treatment.perc.cut, 
                 na.action = na.omit, data = db.19); summary(agb19.m1)

# reduced model
agb19.m2 <- lm(agb.growth.19 ~ 1, 
               na.action = na.omit, data = db.19); summary(agb19.m2)

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/biomass/agb_results/likelihood_ratio_test_results/full_treatment/plot_level/agb19/")
anova(agb19.m1, agb19.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment

# 2. 2018-2023 ----
# full model
agb18.m1 <- lm(agb.growth.18 ~ treatment.perc.cut, 
               na.action = na.omit, data = db.18); summary(agb18.m1)

# reduced model
agb18.m2 <- lm(agb.growth.18 ~ 1, 
               na.action = na.omit, data = db.18); summary(agb18.m2)

# likelihood ratio tests on full and reduced models
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/biomass/agb_results/likelihood_ratio_test_results/full_treatment/plot_level/agb18/")
anova(agb18.m1, agb18.m2) %>% as.data.frame() %>% write.csv(file = "treatment_results.csv") # testing treatment

