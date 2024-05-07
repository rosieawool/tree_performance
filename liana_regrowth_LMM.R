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
tree.data.final <- read.csv("~/Google Drive/My Drive/PhD/chapters/tree_response/Malua_data/formatted_data/pre_post_full_PC/final_data_versions/Tree_diameter_MCCE_merged_300424.csv")
glimpse(tree.data.final)

### Make sure classes corect ###
tree.data.final$coi.23 <- factor(tree.data.final$coi.23, levels = c(0, 1, 2, 3, 4), ordered = TRUE) # Order levels of the factor; otherwise R will alphabetize them
tree.data.final$treatment.perc.cut <- factor(tree.data.final$treatment.perc.cut, levels = c(0, 60, 80, 100), ordered = TRUE)
tree.data.final$Site <- as.factor(tree.data.final$Site)
tree.data.final$Block <- as.factor(tree.data.final$Block)
tree.data.final$Plot <- as.factor(tree.data.final$Plot)

### Separate age classes ###
# at this point, we are removing saplings and seedlings
adult.db <- subset(tree.data.final, Age.class == "adult")
juv.db <- subset(tree.data.final, Age.class == "juvenile")

### Create model datasets ###
# need to do this to remove NAs and make sure db length is the same for transformations, models etc.
# db1: for the models focussing on post_liana_presence
adult.db1 <- adult.db[which(complete.cases(adult.db[,c("Cut", "treatment.perc.cut", "Site", "Block", "Plot", "TPL.name", "post_liana_presence")])),]
juv.db1 <- juv.db[which(complete.cases(juv.db[,c("Cut", "treatment.perc.cut", "Site", "Block", "Plot", "TPL.name", "post_liana_presence")])),]
# post_liana_presence: 1 = any liana presence (coi, prev, canopy liana, stems), 0 = liana absence

# db2: for the model focussing on canopy openness index (coi)
# polr train db (does not require random effects columns)
adult.db2 <- adult.db[which(complete.cases(adult.db[,c("treatment.perc.cut", "coi.23")])),]

# clmm train db (requires random effects columns)
variables_list <- c("Site", "Block", "Plot", "TPL.name", "treatment.perc.cut", "coi.23")
adult.db21 <- adult.db %>% dplyr::select(all_of(variables_list))
adult.db22 <- adult.db21[which(complete.cases(adult.db21[,c("Site", "Block", "Plot", "TPL.name", "treatment.perc.cut", "coi.23")])),]

### Check distributions ###
hist(adult.db$post_liana_presence)
hist(juv.db$post_liana_presence)

# oN_adult <- orderNorm(adult.db2$coi_diff); oN_adult # orderNorm
# MASS::truehist(oN_adult$x.t, main = " transformation", nbins = 12) # because bN chose orderNorm, x.t will be the oN transformation
# adult.db2$coi.23.xt <- bN_adult$x.t


### MODELS ### ----
## binary: post_liana_presence
adult.m1.plp <- glmer(post_liana_presence ~ treatment.perc.cut + (1|Site/Block/Plot) + (1|TPL.name),
                  data = adult.db1, family = "binomial"); summary(adult.m1.plp)
adult.m2.plp <- glmer(post_liana_presence ~ (1|Site/Block/Plot) + (1|TPL.name),
                  data = adult.db1, family = "binomial"); summary(adult.m2.plp)
anova(adult.m1.plp, adult.m2.plp) %>% as.data.frame() %>% write.csv(file = "~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/liana_regrowth/liana_results/likelihood_ratio_test_results/all_spp/liana.adult/liana_presence/treatment_results.csv") # testing treatment

juv.m1.plp <- glmer(post_liana_presence ~ treatment.perc.cut + (1|Site/Block/Plot) + (1|TPL.name),
                  data = juv.db1, family = "binomial"); summary(juv.m1.plp)
juv.m2.plp <- glmer(post_liana_presence ~ (1|Site/Block/Plot) + (1|TPL.name),
                  data = juv.db1, family = "binomial"); summary(juv.m2.plp)
anova(juv.m1.plp, juv.m2.plp) %>% as.data.frame() %>% write.csv(file = "~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/liana_regrowth/liana_results/likelihood_ratio_test_results/all_spp/liana.juv/liana_presence/treatment_results.csv") # testing treatment


## full: coi.23 (only for adults)
# data exploration
table(adult.db2$treatment.perc.cut, adult.db2$coi.23) 

# 1. using polr, which is without the random effects
# divide data into training and test datasets
# random sampling:
samplesize <- 0.6*nrow(adult.db2)
set.seed(100)
index <- sample(seq_len(nrow(adult.db2)), size = samplesize)
datatrain <- adult.db2[index,]
datatest <- adult.db2[-index,]

# model
adult.m1.coi <- polr(coi.23 ~ treatment.perc.cut, data = datatrain, Hess = TRUE); summary(adult.m1.coi) # https://stackoverflow.com/questions/57297771/interpretation-of-l-q-c-4-for-logistic-regression
adult.m2.coi <- polr(coi.23 ~ 1, data = datatrain, Hess = TRUE); summary(adult.m2.coi) 
anova(adult.m1.coi, adult.m2.coi) %>% as.data.frame() %>% write.csv(file = "~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/liana_regrowth/liana_results/likelihood_ratio_test_results/all_spp/liana.adult/coi.23/treatment_polr_norandoms_results.csv") # testing treatment

# compute confusion table and misclassification error
predictcoi <- predict(adult.m1.coi, datatest)
table(datatest$coi.23, predictcoi) 
# many 0s in the confusion matrix, meaning those values are not identified properly in the train model, and this is because there's poor representation of that kind of data in the dataset
mean(as.character(datatest$coi.23) != as.character(predictcoi)) # we find that the misclassification error for our model is 46%

# side quest: plot figure
library("effects")
plot(Effect(focal.predictors = "treatment.perc.cut",adult.m1.coi)) # a figure

# 2. using ordinal and clmm, which is a cumulative link mixed model and incorporates the random effects too

# divide data into training and test datasets
# random sampling:
samplesize <- 0.6*nrow(adult.db22)
set.seed(100)
index <- sample(seq_len(nrow(adult.db22)), size = samplesize)
datatrain2 <- adult.db22[index,]
datatest2 <- adult.db22[-index,]

# model
library(ordinal)
adult.m3.coi <- clmm(coi.23 ~ treatment.perc.cut + (1|Site/Block/Plot) + (1|TPL.name), data = adult.db22, Hess = TRUE, na.action = na.omit); summary(adult.m3.coi)
adult.m4.coi <- clmm(coi.23 ~ 1 + (1|Site/Block/Plot) + (1|TPL.name), data = adult.db22, Hess = TRUE, na.action = na.omit); summary(adult.m4.coi)
anova(adult.m3.coi, adult.m4.coi) %>% as.data.frame() %>% write.csv(file = "~/Google Drive/My Drive/PhD/chapters/tree_response/analysis/models/liana_regrowth/liana_results/likelihood_ratio_test_results/all_spp/liana.adult/coi.23/treatment_clmm_withrandoms_results.csv") # testing treatment


