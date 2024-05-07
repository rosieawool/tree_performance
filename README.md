# tree_performance repository 

## Scripts
### Relative growth rate (RGR) scripts
##### M1_RGR_calculations.R features how I calculated RGR, either as cm/year or cm/month growth per tree
##### M2_RGR_LMM.R features the linear mixed effect models used to examine the effects of multiple variables on tree RGR

### Aboveground biomass (AGB) scripts
##### M1_AGB_calculations.R features how I calculated AGB, with explanations of methodology at the top of the script
##### M1_AGB_calculations_v2.R is an updated way that I'd like to calculate AGB as it is more in-depth so likely more accurate, however this is a WIP
##### M2_AGB_LMM_trees.R features the linear mixed effect models used to examine the effects of multiple variables on tree AGB
##### M2_AGB_LMM_plot.R features the linear mixed effect models used to examine the effects of treatment variables on plot-level AGB, fixed effects are limited here as most variables cannot be aggregated to a plot level

### Liana regrowth script
##### liana_regrowth_LMM.R features multiple models, including GLMs that studies the effect of treatment on post-liana presence (i.e. ANY lianas recorded, either through COI, prevalence etc.), and POLR and CLMM models that examine the effects of treatment on Canopy Openness Index (COI) in 2023

## Data
##### Tree_diameter_MCCE_merged_300424.csv features the raw tree data
##### column_descriptions.csv describes what each column refers to in the Tree_diameter_MCCE_merged_300424.csv file
