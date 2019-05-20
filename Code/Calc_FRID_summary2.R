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
  write.csv(f1, paste("TabsFigs/",forest," Table 1.csv", sep=""), row.names = F)
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
  ggsave(paste("Code/TabsFigs/",forest," CCstackedbar.jpg"), plot=p1,
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
    geom_text(aes(label = paste0(Percent, "%")), 
              position = position_stack(vjust = 0.5)) +
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
  ggsave(paste("Code/TabsFigs/",forest," CCpiechart.jpg"), plot=p1,
         width = 7, height = 4.5, units = 'in')
}


 
 
 # Could I do a sankey diagram?
 ggplot(as.data.frame(UCBAdmissions),
        aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
   geom_alluvium(aes(fill = Admit), width = 1/12) +
   geom_stratum(width = 1/12, fill = "black", color = "grey") +
   geom_label(stat = "stratum", label.strata = TRUE) +
   scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
   scale_fill_brewer(type = "qual", palette = "Set1") +
   ggtitle("UC Berkeley admissions and rejections, by sex and department")
  
