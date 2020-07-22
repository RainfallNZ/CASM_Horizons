#Script to prepare Soluble Inorganic Nitrogen plots
#Tim Kerr
#Rainfall.NZ
#July 2020

#This script estimates the Soluble Inorganic Nitrogen (SIN) from estimates of Total Nitrogen.
#Total Nitrogen concentrations have been prepared using Streamlined Environmental's CASM model.
#This has been done for a series of scenarios, including a "baseline" scenario.
#SIN has been estimated for the same region by Caroline Fraser for a baseline scenario.
#It is assumed that the relative change in Total Nitrogen between a scenario and the "Baseline" scenario will be the same as the relative change in SIN.
#This was caried out for each scenario.
#The relative change was added to the baseline SIN to detemine the scenario SIN
#The SIN value with respect to water quality targets was established for each Water Management SUb Zone
#A categorical comply/not comply was detemined and plotted.
#The percent difference from the targets was also plotted.

#Load libraries
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(sf)) install.packages("sf"); library(sf)
if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)                #needed to load excel data

#Set directories
ProjectDirectory <- "D:\\Projects\\LWP\\Horizons"
DataDirectory    <- file.path(ProjectDirectory,"Data")
CASMDirectory    <- file.path(ProjectDirectory,"CASM")
GISDirectory     <- file.path(ProjectDirectory,"Data\\GIS")
DiagramDirectory <- file.path(ProjectDirectory,"Reports","Diagrams")

#Set filenames
SINDataFile      <- file.path(DataDirectory,"AssessmentPointCriteriaAndConcentrations.xlsx")
ScenarioDataFile <- file.path(CASMDirectory,"ScenarioOutcomeWMSZSummary.csv")
SubZoneShapeFile <- file.path(GISDirectory,"Water_Management_Subzones_cleaned","Water_Management_Subzones_cleaned.shp")
PointShapeFile   <- file.path(GISDirectory,"CASMOutputAsPoints","CASMOutputAsPoints.shp")

#Load shape files
SubZoneSpatial   <- read_sf(SubZoneShapeFile)
PointData        <- read_sf(PointShapeFile)

#Open data files
SINData          <- read.xlsx(SINDataFile,sheet = "AssessmentPointCriteriaAndConce")
ScenarioData     <- read.csv(ScenarioDataFile)

#Stick the SIN data to the Scenario Data and ditch the unwanted cenario Columns
AllData <- merge(SINData[,c("WMSZ","Criteria.SIN.(g/m3)","Predicted.current.Mean.SIN.(Fraser.and.Snelder.2020)")],
                 ScenarioData[,c("WMSZ","Baseline","Scenario0_a" ,"Scenario0_b", "Scenario1_a", "Scenario1_b", "Scenario2_a", "Scenario2_b", "Scenario2_c",
                                 "Scenario2_d" ,"Scenario3_e", "Scenario3_f", "Scenario3_g", "Scenario3_h", "X4","X5","X6","X7","X8","X9a","X9b","X10","Ton")])

#Calculate relative change in TN
Scenarios <- c("Baseline","Scenario0_a" ,"Scenario0_b", "Scenario1_a", "Scenario1_b", "Scenario2_a", "Scenario2_b", "Scenario2_c",
               "Scenario2_d" ,"Scenario3_e", "Scenario3_f", "Scenario3_g", "Scenario3_h", "X4","X5","X6","X7","X8","X9a","X9b","X10","Ton")

SINDifferenceMatrix <- sapply(Scenarios, function(Scenario){
  #Calculate scenario SIN
  #Calculate relative change in TN
  RelativeDeltaTN <- (AllData[,Scenario] - AllData[,"Baseline"])/AllData[,"Baseline"]
  
  #Calculate change in SIN
  ScenarioSIN <-  (RelativeDeltaTN + 1) * AllData[,"Predicted.current.Mean.SIN.(Fraser.and.Snelder.2020)"]
  
  #Calculate difference from the target SIN
  DeltaTargetSINPercent <- (ScenarioSIN - AllData[,"Criteria.SIN.(g/m3)"])/AllData[,"Criteria.SIN.(g/m3)"] * 100
  
  #Calculate whether Scenario SIN is better or worse than the target
  #BeatenSINTarget <- DeltaTargetSINPercent <= 0
  
  return(DeltaTargetSINPercent)
  
})

#Turn into a data frame with the water management subzones names
SINDifferences <- cbind(Node.Name=AllData$WMSZ,as.data.frame(SINDifferenceMatrix))

#Function to map the export coefficients and river nutrient concentrations for a scenario 

SINMap <- function(PercentDifFromTargetSIN = SINDiffData,
                        NodeAreas = SubZoneSpatial, 
                        NodeOutlets = PointData, 
                        ImageName = "Filename.png"){
  #Load required libraries
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)   
  if (!require(raster)) install.packages("raster"); library(raster)
  #if (!require(plyr)) install.packages("plyr"); library(plyr)
  if (!require(ggplot2 )) install.packages("ggplot2 "); library(ggplot2 )
  if (!require(sf)) install.packages("sf"); library(sf)
  if (!require(ggspatial)) install.packages("ggspatial"); library(ggspatial) #Enables addition of scale bar and north arrow to maps
  if (!require(patchwork)) install.packages("patchwork"); library(patchwork) #Enables side by side map plotting
  if (!require(ggsn)) install.packages("ggsn"); library(ggsn) #has a North arrow function
  if (!require(ggmap)) install.packages("ggmap"); library(ggmap) #help with mapping
  if (!require(GISTools)) install.packages("GISTools"); library(GISTools) #help with mapping
  if (!require(ggthemes)) install.packages("ggthemes"); library(ggthemes) #help with mapping
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis) #help with mapping
  if (!require(ggpubr)) install.packages("ggpubr"); library(ggpubr) #help with mapping

  #SIN difference as compliance or not
    SINTargetData <- PercentDifFromTargetSIN %>% 
      dplyr::rename(SINPctDiff = 2) %>%
      mutate(Compliance = SINPctDiff <= 0) %>%
      mutate(PercentChange = pmin(50,pmax(-50,SINPctDiff)))
    
  NodeAreas<- NodeAreas %>% merge(SINTargetData,by.x="Zone_Code",by.y="Node.Name")

  #Add the scenario data to the zone spatial data
  NodeOutlets<- NodeOutlets %>% merge(SINTargetData,by.x="WMSZ",by.y="Node.Name")
  
  theme_set(theme_map())
  MapExtent <- extent(NodeAreas)  
  
  #Create the SIN target compliance map
  ComplianceMap <- ggplot() +
    #Add a grey background
    geom_sf(data = NodeAreas, fill="#bcbcbc",size=0.06)+      
    #Set the position, size and title allignment of the legend
    theme(legend.position = c(0.7,0.73),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5) +
    
    #Add the Chlorpleth map for the change in export coefficients
    #geom_sf(data = NodeAreas, aes(fill = Compliance))+
      geom_sf(data = NodeAreas, aes(fill = factor(Compliance)))+
    # coord_sf(crs = "+init=epsg:2193")+
    
    #Add the node area boundaries, and the legend formatting details the coordinate system and extent
    scale_fill_manual(name=NULL,values = c("#d7191c",  "#2b83ba"),labels = factor(c("Noncompliant","Compliant"))) +
    #scale_fill_discrete(labels = factor(c("Noncompliant","Compliant")))+
    #scale_fill_gradientn("Some title",na.value = NA,
    #                     values = c(0,1),
    #                     colours=c("#ffdf9a","#d7191c")) + 
    #Set the legend labels to be on the left
    #guides(fill = guide_colourbar(label.position = "left")) +
    guides(fill = guide_legend(label.position = "left",reverse=TRUE)) +
    #add the node area outlines
    geom_sf(data = NodeAreas,fill = "transparent",size = 0.06) + 
    coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1],MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4]))
  

  
  SINTargetPctDiferenceMap <- ggplot(data = NodeAreas) + 
    #theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.35)+
    theme(legend.position = "bottom",legend.key.height = unit(0.6,"cm"),legend.title.align=0.5)+
    geom_sf(fill = "#bcbcbc",size = 0.06)  + 
    geom_sf(data = NodeOutlets, aes(fill=PercentChange),col="black", size=3,pch=21)+
    scale_fill_gradientn(limits =c(-50,50),"Percentage (%)",na.value = NA,
                         values = c(0,0.49,0.51,1),
                         #colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + #Reversed colours
                         colours=c("#2b83ba","#def2b4","#ffdf9a","#d7191c")) + 
    guides(fill = guide_colourbar(label.position = "bottom")) +
    north(SubZoneSpatial,"bottomleft",anchor = c(x =1727500, y = 5510000 ),scale = 0.15) +
    #scalebar(data = NodeAreas, dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
    #                          location = "bottomleft",st.dist=0.03,st.size=3) +
    #scalebar(dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
    #         location = "bottomleft",st.dist=0.03,st.size=3,x.min=MapExtent[1]-30000,x.max=MapExtent[2],y.min=MapExtent[3],y.max=MapExtent[4]) +
    #coord_sf(crs = "+init=epsg:2193")
    #Set the coordinate system and extent
    #coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = MapExtent[3:4])
    coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]) + 10000,ylim = c(MapExtent[3],MapExtent[4])) +
    labs(title = "Difference from SIN Target")
  bob <- get_legend(SINTargetPctDiferenceMap)
  #DoubleMap <- ComplianceMap + SINTargetPctDiferenceMap+ plot_layout(ncol = 2)
  #TripleMap <- SINTargetPctDiferenceMap + SINTargetPctDiferenceMap + SINTargetPctDiferenceMap + plot_layout(ncol = 3)
  TripleMap <- ggarrange(SINTargetPctDiferenceMap, SINTargetPctDiferenceMap, SINTargetPctDiferenceMap, ncol = 3)

  
  return(TripleMap)
}


SINMapOutput <- SINMap(PercentDifFromTargetSIN = SINDifferences[,c("Node.Name","Baseline")])
#png(file.path(DiagramDirectory,"SINMapTest.png"),width = 160, height = 120, units = "mm",res = 300)
#print(SINMapOutput)

#dev.off()
