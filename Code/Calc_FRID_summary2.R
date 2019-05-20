################################################################################
# Extract data needed for FRID summaries
################################################################################
rm(list = ls())
library(tidyverse); theme_set(theme_bw(base_size=9))
library(rgdal)

#Set workinng directory - change this file path to wherever you have FRID data
setwd("~/PMF/Details/SoCal/FRID_summaries")

#Read in merged FRID data
frid <- read.csv("FRIDout.csv")
frid <- frid[!is.na(frid$AREA_GEO),] # cut slivers with 0 area

# Get standard data for each PFR
PFRdata <- frid %>%
  group_by(PFR) %>%
  summarize_at(vars(meanRefFRI:maxRefFRI), mean)

# Do forest-level summary first-------------------------------------------------
# Calculate mean PFRID and CC by forest and PFR + % of area
frid2 <- frid %>% filter(OWNERCLASS=="USDA FOREST SERVICE" & PFR!="none") %>%
  group_by(FORESTNAME) %>%
  mutate(TotalAcres=sum(AREA_GEO)) %>%
  group_by(FORESTNAME,PFR,TotalAcres) %>%
  # Mean PFRID should be weighted by area
  summarize(`Mean PFRID`=round(weighted.mean(meanPFRID,AREA_GEO)),
            `Presettlement FRI (yrs)`=mean(meanRefFRI),
            `Current FRI (yrs)`=round(weighted.mean(currentFRI,AREA_GEO)),
            Acres=sum(AREA_GEO)) %>%
  mutate(`% of area analyzed`=round(Acres/TotalAcres*100)) %>%
  select(FORESTNAME,PFR, `Presettlement FRI (yrs)`, `Current FRI (yrs)`,
         `Mean PFRID`, `% of area analyzed`) %>%
  filter(`% of area analyzed`>=1)

# Loop through forests and spit out tables
Forests = unique(frid$FORESTNAME)
for (forest in Forests){
  f1 <- filter(frid2, FORESTNAME==forest) %>%
    ungroup() %>%
    select(-FORESTNAME) %>%
    arrange(desc(`Mean PFRID`))
  write.csv(f1, paste("TabsFigs/",forest," Table 1.csv", sep=""), row.names = F)
}

# Calculate % forest in each CC and % of total in diff regimes-------------------

  

  
