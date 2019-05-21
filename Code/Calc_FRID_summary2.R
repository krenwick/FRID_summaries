################################################################################
# Extract data needed for FRID summaries
################################################################################
rm(list = ls())
library(tidyverse); theme_set(theme_bw(base_size=9))
#library(rgdal)
library(sf)
library(ggspatial) # for scale bar and north arrow

#Set workinng directory - path to location with FRIDout.csv
setwd("~/PMF/Details/SoCal/FRID_summaries")

#Read in merged FRID data
frid <- read.csv("FRIDout.csv")
frid <- frid[!is.na(frid$AREA_GEO),] # cut slivers with 0 area

# Read in shapefile
map <- st_read("C:/GIS/FRID17/FRID_justFS.shp")

# Read in FS boundary to make map prettier
boundary <- st_read("C:/GIS/BasicOwnership_SoCalClip.shp")

# Get standard data for each PFR
PFRdata <- frid %>%
  group_by(PFR) %>%
  summarize_at(vars(meanRefFRI:maxRefFRI), mean) %>%
  filter(PFR!="none")

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
  forest2 <- gsub(" ", "", forest, fixed = TRUE)
  write.csv(f1, paste("TabsFigs/",forest2,"Table1.csv", sep=""), row.names = F)
}

# Calculate % forest in each CC and % of total in diff regimes-------------------
byCC <- frid %>% filter(OWNERCLASS=="USDA FOREST SERVICE" & PFR!="none") %>%
  group_by(FORESTNAME) %>%
  mutate(TotalAcres=sum(AREA_GEO)) %>%
  group_by(FORESTNAME,PFR,meanCC_FRI,TotalAcres) %>%
  summarize(Acres=sum(AREA_GEO)) %>%
  mutate(`% of area analyzed`=round(Acres/TotalAcres*100)) %>%
  select(FORESTNAME,PFR, meanCC_FRI, `% of area analyzed`) %>%
  group_by(FORESTNAME, meanCC_FRI) %>%
  mutate(percfores=sum(`% of area analyzed`)) %>%
  filter(`% of area analyzed`>=1) %>%
  mutate(meanCC=as.factor(meanCC_FRI)) %>%
  mutate(`Condition Class`=ifelse(meanCC=="1"|meanCC=="-1", "1/-1", meanCC))
byCC$`Condition Class` = factor(byCC$`Condition Class`, ordered=T,
                                levels=c("3","2","1/-1","-2", "-3"))
  
SB <- filter(byCC, FORESTNAME=="San Bernardino National Forest")  

# TRY: stacked bar chart for PFR
for (forest in Forests){
  f1 <- filter(byCC, FORESTNAME==forest)
  p1 <- ggplot(data=f1, aes(y=`% of area analyzed`, x=PFR)) +
    geom_bar(stat="identity", aes(fill=as.factor(`Condition Class`))) +
    scale_fill_manual(name="Condition Class", 
          values=c("red3","yellow","palegreen", "turquoise2","royalblue4"),
          labels=c("3 (Too little fire)","2","1/-1","-2","-3 (Too much fire)")) +
    coord_flip() +
    ylab("% of Forest") +
    xlab("Presettlement Fire Regime Group")
  forest2 <- gsub(" ", "", forest, fixed = TRUE)
  ggsave(paste("Code/TabsFigs/",forest2,"CCstackedbar.jpg", sep=""), plot=p1,
         width = 7, height = 4.5, units = 'in')
}

# Make pie chart of CC
byCC2 <- byCC %>%
  group_by(FORESTNAME, `Condition Class`) %>%
  summarize(Percent=sum(`% of area analyzed`))
SB2 <- filter(byCC2, FORESTNAME=="San Bernardino National Forest") 


# Make pie chart for each forest
for (forest in Forests){
  f1 <- filter(byCC2, FORESTNAME==forest)
  p1 <- ggplot(data=f1, aes(x="", y=Percent,fill=`Condition Class`)) +
    geom_bar(stat="identity", color="black") +
    coord_polar("y", start=0) +
    geom_text(aes(x=1.55,label = paste0(Percent, "%")), 
              position = position_stack(vjust = 0.5), color="black") +
    scale_fill_manual(name="Condition Class", 
                      values=c("red3","yellow","palegreen", "turquoise2","royalblue4"),
                      labels=c("3 (Too little fire)","2","1/-1","-2","-3 (Too much fire)")) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_classic() + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position="right",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,0,-10,-20))
  forest2 <- gsub(" ", "", forest, fixed = TRUE)
  ggsave(paste("Code/TabsFigs/",forest2,"CCpiechart.jpg", sep=""), plot=p1,
         width = 7, height = 4.5, units = 'in')
}

# Make a map for each forest
# trying out new dev version of ggplot2, which works with sf objects
#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)

SB <- map %>% filter(FORESTNAME=="San Bernardino National Forest"&
                       meanCC_FRI!=-999) %>%
  mutate(meanCC=as.character(meanCC_FRI)) %>%
  mutate(CC=ifelse(meanCC=="1"|meanCC=="-1", "1/-1", meanCC)) %>%
  mutate(CC=factor(CC, ordered=T,
                     levels=c("3","2","1/-1","-2", "-3")))
B_SB <- filter(boundary, FORESTNAME=="San Bernardino National Forest" &
                 OWNERCLASS=="USDA FOREST SERVICE")
         
ggplot(data=SB) +
  geom_sf(aes(fill=CC), color=NA) +
  theme_bw() +
  scale_fill_manual(name="Condition Class", 
        values=c("red3","yellow","palegreen", "turquoise2","royalblue4"),
        labels=c("3 (Too little fire)","2","1/-1","-2","-3 (Too much fire)")) +
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank()) +
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       legend.position = "right") +
  #       #legend.key = element_blank())
  geom_sf(data=B_SB, color="black", fill=NA) +
  annotation_scale(location = "bl", width_hint = .2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.09, "in"), pad_y = unit(0.09, "in"),
                         height = unit(.3, "in"), width = unit(.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  ggtitle("2017 Percent Fire Return Interval Departures (PFRID): San Bernadino NF")


# Could I do a sankey diagram?
 ggplot(as.data.frame(UCBAdmissions),
        aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
   geom_alluvium(aes(fill = Admit), width = 1/12) +
   geom_stratum(width = 1/12, fill = "black", color = "grey") +
   geom_label(stat = "stratum", label.strata = TRUE) +
   scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
   scale_fill_brewer(type = "qual", palette = "Set1") +
   ggtitle("UC Berkeley admissions and rejections, by sex and department")
  
