# Script: M1_RGR_calculations
# Script to be run before any RGR model scripts

#### SET UP #### ----
### Read in packages ### 
library(dplyr)
library(timeplyr)
library(data.table)

### Read in data ### 
tree.data.final <- read.csv("~/Google Drive/My Drive/PhD/chapters/tree_response/Malua_data/formatted_data/pre_post_full_PC/final_data_versions/Tree_diameter_MCCE_merged_300424.csv")
glimpse(tree.data.final)

### Make Site, Block and Plot very clearly categorical ###
#tree.data.final$Site<-sub("^","Site",tree.data.final$Site)
#tree.data.final$Block<-sub("^","Block",tree.data.final$Block)
#tree.data.final$Plot<-sub("^","Plot",tree.data.final$Plot)

### Add in RGR columns ### 
# Calculate number of years (or months) between pre- and post-data collections
tree.data.final$date.18 <- as.Date(tree.data.final$date.18, "%d/%m/%Y")
tree.data.final$date.19 <- as.Date(tree.data.final$date.19, "%d/%m/%Y")
tree.data.final$date.23 <- as.Date(tree.data.final$date.23, "%d/%m/%Y")
tree.data.final$time.diff.18 <- time_diff(tree.data.final$date.18, tree.data.final$date.23, "years")
tree.data.final$time.diff.19 <- time_diff(tree.data.final$date.19, tree.data.final$date.23, "years")

# RGR - relative growth rate calculation
tree.data.final$diam.23 <- as.numeric(tree.data.final$diam.23)
tree.data.final$Diam.18 <- as.numeric(tree.data.final$Diam.18)
tree.data.final$Diam.19 <- as.numeric(tree.data.final$Diam.19)
tree.data.final$Diam <- as.numeric(tree.data.final$Diam) # this is the diameter column for 2018 and 2019

# rgr 2018
tree.data.final$rgr.18 <- (log(tree.data.final$diam.23) - log(tree.data.final$Diam.18)) / as.numeric(tree.data.final$time.diff.18) # this means that rgr is logged

# rgr 2019
tree.data.final$rgr.19 <- (log(tree.data.final$diam.23) - log(tree.data.final$Diam.19)) / as.numeric(tree.data.final$time.diff.19) 

# combined RGR for 2018 + 2019
tree.data.final$rgr.all <- apply(cbind(tree.data.final$rgr.18, tree.data.final$rgr.19), 1, function(x) ifelse(all(is.na(x)), NA, sum(x,na.rm=T)))
# this is fine as 18 and 19 are exclusive so never do you get rgr value for both at same time
# this code also means that when both rgr.month.18 and 19 are NA, rgr.month.all will be NA (not 0 like with rowSums) 

