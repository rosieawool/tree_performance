# Script: M1_AGB_calculations
# To be read before running M2_RGR_LMM

### SET UP ### ----

## Calculation process ----
# Final equation used (taken from Lai et al., 2017):
# AGB growth = (AGB gains from growth of trees recorded in pre that survived until post (surv) / time period between pre and post) + (AGB gains from trees recruited (recr) / time period between pre and post)
# ABG growth unit should be weight / area / time (e.g. kg/km^2/month) so will calculate at the plot level

# AGB equation (taken from Kenzo et al., 2009, modified from Yamakura et al., 1986):
# ln(Wt) = 2.62 * ln(DBH) - 2.3
# Wt = dry weight (kg)
# This method was used in other Bornean studies including Phua et al. 2014 and Ioki et al. 2014.
# Kenzo comments on the failures of Chave's equations for this area, and van Bruegel et al. 2011 is only applicable to Panaman studies.

## Read in packages
library(dplyr)

## Read in data
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/Malua_data/formatted_data/pre_post_full_PC/final_data_versions/")
tree.data.final <- read.csv("Tree_diameter_MCCE_merged_300424.csv")
glimpse(tree.data.final)

## Organise df
# Re-classify columns
tree.data.final$Diam.18 <- as.numeric(tree.data.final$Diam.18)
tree.data.final$Diam.19 <- as.numeric(tree.data.final$Diam.19)
tree.data.final$diam.23 <- as.numeric(tree.data.final$diam.23)

# Calculate number of years (or months) between pre- and post-data collections
tree.data.final$date.18 <- as.Date(tree.data.final$date.18, "%d/%m/%Y")
tree.data.final$date.19 <- as.Date(tree.data.final$date.19, "%d/%m/%Y")
tree.data.final$date.23 <- as.Date(tree.data.final$date.23, "%d/%m/%Y")
tree.data.final$time.diff.18 <- time_diff(tree.data.final$date.18, tree.data.final$date.23, "years")
tree.data.final$time.diff.19 <- time_diff(tree.data.final$date.19, tree.data.final$date.23, "years")

# Subset
# only keep adults and juveniles, and remove all dead, errored, and recruits
target.age <- c("juvenile", "adult")
tree.data.final.surv <- tree.data.final %>%
  filter(Age.class %in% target.age) %>%
  filter(remove_rgr != "1") # 8357 rows
variables_list <-
  c(
    "unique.ID",
    "Plot",
    "Diam.18",
    "Diam.19",
    "diam.23",
    "time.diff.18",
    "time.diff.19"
  ) # just take columns needed
diams.surv <-
  tree.data.final.surv %>% dplyr::select(all_of(variables_list))

# only keep adults and juveniles, and recruits
tree.data.final.recruit <- tree.data.final %>%
  filter(Age.class %in% target.age) %>%
  filter(why_remove == "new recruit 23")
diams.recruit <-
  tree.data.final.recruit %>% dplyr::select(all_of(variables_list))

## 1. Calculations of the survived
# Calculate AGB per tree in 2018/19 and 2023
diams.surv$agb.23 <- 2.62 * log(diams.surv$diam.23) - 2.3
diams.surv$agb.18 <- 2.62 * log(diams.surv$Diam.18) - 2.3
diams.surv$agb.19 <- 2.62 * log(diams.surv$Diam.19) - 2.3

# Calculate ABG per month per tree
diams.surv$agb.growth.18 <-
  (diams.surv$agb.23 - diams.surv$agb.18) / diams.surv$time.diff.18
diams.surv$agb.growth.19 <-
  (diams.surv$agb.23 - diams.surv$agb.19) / diams.surv$time.diff.19

## 2. Calculations of the recruited
diams.recruit$agb.recruit <- 2.62 * log(diams.recruit$diam.23) - 2.3

# Calculate AGB per month per tree for recruits (used mean of all time.diff.19 for number of months (time_diff) for recruits, which is 48.53 weeks, and will only include this in agb 19 analysis)
diams.recruit$agb.growth.19 <- diams.recruit$agb.recruit / 48.53

## 3. Left join diams.surv and diam.recruit, then remove unwanted cols
diams <- full_join(diams.surv, diams.recruit)
colnames(diams)
drop.cols <-
  c(
    "Diam.18",
    "Diam.19",
    "diam.23",
    "time.diff.18",
    "time.diff.19",
    "agb.23",
    "agb.18",
    "agb.19",
    "agb.recruit"
  )
agb.growth.trees <- diams %>% dplyr::select(-one_of(drop.cols))

## 4. Aggregate to plot level
agb.growth.plot <-
  stats::aggregate(
    agb.growth.trees[, 3:4], # the [, 3:4] is needed to remove the unique.ID & Plot cols from the sum calculations
    by = list(agb.growth.trees$Plot),
    na.rm = T,
    FUN = sum
  )
agb.growth.plot <- agb.growth.plot %>% dplyr::rename(Plot = Group.1)
agb.growth.plot$agb.growth.18[agb.growth.plot$agb.growth.18 == 0] <- NA # aggregate made NAs sum to 0 so need to change to NAs
agb.growth.plot$agb.growth.19[agb.growth.plot$agb.growth.19 == 0] <- NA

# agb growth of trees [kg/month]
# agb growth at plot level [kg/month/plot] where each plot was 20m^2

