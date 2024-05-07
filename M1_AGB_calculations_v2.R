### SET UP ### ----

## Calculation process ----
# Final equation used (taken from Lai et al., 2017):
# AGB growth = (AGB gains from growth of trees recorded in pre that survived until post (surv) / time period between pre and post) + (AGB gains from trees recruited (recr) / time period between pre and post)
# ABG growth unit should be weight / area / time (e.g. kg/km^2/month) so will calculate at the plot level

# AGB is calculated using the BIOMASS package, which utilises the Chave et al. 2014 allometric equation
# For more information, check out: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12753

## Read in packages
library(dplyr)
library(BIOMASS)

## Read in data
setwd("~/Google Drive/My Drive/PhD/chapters/tree_response/Malua_data/formatted_data/pre_post_full_PC/final_data_versions/")
tree.data.final <- read.csv("Tree_diameter_MCCE_merged_300424.csv")
glimpse(tree.data.final)

## Let's just take what we need
# Calculate number of years (or months) between pre- and post-data collections
tree.data.final$date.18 <- as.Date(tree.data.final$date.18, "%d/%m/%Y")
tree.data.final$date.19 <- as.Date(tree.data.final$date.19, "%d/%m/%Y")
tree.data.final$date.23 <- as.Date(tree.data.final$date.23, "%d/%m/%Y")
tree.data.final$time.diff.18 <- time_diff(tree.data.final$date.18, tree.data.final$date.23, "years")
tree.data.final$time.diff.19 <- time_diff(tree.data.final$date.19, tree.data.final$date.23, "years")

# For the survived, we remove the dead (as well as other rows listed under why_remove) and only keep juv and adults
target.age <- c("juvenile", "adult")
tree.data.final.surv <- tree.data.final %>%
  filter(Age.class %in% target.age) %>%
  filter(remove_rgr != "1") # 8357 rows
variables_list <- c(
  "unique.ID", 
  "New.Genus", 
  "New.Species", 
  "TPL.name", 
  "treatment.perc.cut",
  "Plot",
  "Diam.18",
  "Diam.19",
  "diam.23",
  "time.diff.18",
  "time.diff.19")
wwn.alive <- tree.data.final.surv %>% dplyr::select(all_of(variables_list))

# For the recruits, we only keep adults and juveniles and recruits
tree.data.final.recruit <- tree.data.final %>%
  filter(Age.class %in% target.age) %>%
  filter(why_remove == "new recruit 23")
wwn.recruit <- tree.data.final.recruit %>% dplyr::select(all_of(variables_list))



### BIOMASS PACKAGE PRE-REQUISITES ### ----
## Let's correct the incorrect taxonomies
# For survived...
Taxo <- BIOMASS::correctTaxo(genus = wwn.alive$New.Genus, species = wwn.alive$New.Species)
# For recruits...
Taxo2 <- BIOMASS::correctTaxo(genus = wwn.recruit$New.Genus, species = wwn.recruit$New.Species)

## Let's get the wood densities for each spp.
# For survived...
WDdata <- BIOMASS::getWoodDensity(genus = Taxo$genusCorrected, species = Taxo$speciesCorrected,
                                  stand = wwn.alive$Plot) 
# For recruits...
WDdata2 <- BIOMASS::getWoodDensity(genus = Taxo2$genusCorrected, species = Taxo2$speciesCorrected,
                                   stand = wwn.recruit$Plot) 
# stand as plot means that any unknown wood densities for spp. will just take the average wood density
# for that plot.

## Retrieve tree height
# For survived...
Hmodel.18 <- BIOMASS::retrieveH(D = wwn.alive$Diam.18, region = "SEAsia")
Hmodel.19 <- BIOMASS::retrieveH(D = wwn.alive$Diam.19, region = "SEAsia")
Hmodel.23 <- BIOMASS::retrieveH(D = wwn.alive$diam.23, region = "SEAsia")
# For recruits...
Hmodel2.23 <- BIOMASS::retrieveH(D = wwn.recruit$diam.23, region = "SEAsia")




### AGB CALCULATION ### ----
## Calculate AGB 2018, 2019, 2023
# For survived...
wwn.alive$AGB.18 <- BIOMASS::computeAGB(D = wwn.alive$Diam.18, WD = WDdata$meanWD, H = Hmodel.18$H)
wwn.alive$AGB.19 <- BIOMASS::computeAGB(D = wwn.alive$Diam.19, WD = WDdata$meanWD, H = Hmodel.19$H)
wwn.alive$AGB.23 <- BIOMASS::computeAGB(D = wwn.alive$diam.23, WD = WDdata$meanWD, H = Hmodel.23$H)
# For recruits...
wwn.recruit$AGB.recruit <- BIOMASS::computeAGB(D = wwn.recruit$diam.23, WD = WDdata2$meanWD, H = Hmodel2.23$H)

## Calculate AGB change
# For survived...
wwn.alive$agb.growth.18 <- ((wwn.alive$AGB.23 - wwn.alive$AGB.18) / wwn.alive$AGB.18) * 100
wwn.alive$agb.growth.19 <- ((wwn.alive$AGB.23 - wwn.alive$AGB.19) / wwn.alive$AGB.19) * 100
# For recruits... (I used mean of all time.diff.19 for number of months (time_diff) for recruits, which is 48.53 weeks, and will only include this in agb 19 analysis))
wwn.recruit$agb.growth.19 <- wwn.recruit$AGB.recruit / 48.53


### MERGE SURVIVED AND RECRUITED ### ----
diams <- full_join(wwn.alive, wwn.recruit)
colnames(diams)
drop.cols <-
  c(
    "Diam.18",
    "Diam.19",
    "diam.23",
    "time.diff.18",
    "time.diff.19",
    "AGB.23",
    "AGB.18",
    "AGB.19",
    "AGB.recruit",
    "New.Genus",
    "New.Species",
    "TPL.name"
  )
agb.growth.trees <- diams %>% dplyr::select(-one_of(drop.cols))




### Aggregate to plot level ### ----
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


# agb growth of trees [Mg/month/tree]
# agb growth at plot level [Mg/month/plot], where each plot was 20m^2

