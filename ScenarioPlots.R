#Script to map diferences between scenario data and output
#The intention is to provide side-by-side maps of the change in export coefficients (left map) and the change in river nutrient concentrations (right map)
#The river concentrations come from the scenario output summary data compiled by Ton from TIm Cox's output.
#The export coefficient data comes from either the CASM input data, combined with the Water Management sub zone spatial data, or, 
#if available for both scenarios, the raster difference in leach rates.
#load libraries
if (!require(rgdal)) install.packages("rgdal"); library(rgdal)   
if (!require(raster)) install.packages("raster"); library(raster)
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(sf)) install.packages("sf"); library(sf)
if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)                #needed to load excel data



#' Function to get the mean export coefficient for each water management sub zone from the "Diffuse Inputs" sheet in a CASM Input spreadsheet
#' @param CASMInputFile The CASM input spreadsheet filename
#' @return A data frame with two columns, The first called "Node.Name", the second with the export coefficients in it (rounded to 1 decimal place)
#' @export
WMSZMeanExportCoeffCalculator <- function(CASMInputFile = ReferenceDataFile){
  #browser()
  InputData <- read.xlsx(CASMInputFile,sheet = "Diffuse Inputs")[,c("Node.Name","Land.Area.(ha)","Export.Coeff.(kg/ha/yr)")]
  InputData$Node.Name <- sub("-.*$","",InputData$Node.Name)
  InputData$Load <- InputData$`Export.Coeff.(kg/ha/yr)` * InputData$`Land.Area.(ha)`
  AggregatedData <- aggregate(InputData[,c("Load","Land.Area.(ha)")],list(InputData$Node.Name),sum)
  AggregatedData$`Export.Coeff.(kg/ha/yr)` <- round(AggregatedData$Load / AggregatedData$`Land.Area.(ha)`,1)
  AggregatedData <- dplyr::rename(AggregatedData, Node.Name = 1) %>% dplyr::select("Node.Name","Export.Coeff.(kg/ha/yr)")
  return(AggregatedData)
}

#' Transform raster as data.frame to be later used with ggplot
#' Modified from rasterVis::gplot
#'
#' @param x A Raster* object
#' @param maxpixels Maximum number of pixels to use
#'
#' @details rasterVis::gplot is nice to plot a raster in a ggplot but
#' if you want to plot different rasters on the same plot, you are stuck.
#' If you want to add other information or transform your raster as a
#' category raster, you can not do it. With `SDMSelect::gplot_data`, you retrieve your
#' raster as a tibble that can be modified as wanted using `dplyr` and
#' then plot in `ggplot` using `geom_tile`.
#' If Raster has levels, they will be joined to the final tibble.
#'
#' @export

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}


#Function to map difference between rasters 

DifferenceMap <- function(ReferenceExportCoefficients = ReferenceRaster,
                          ComparisonExportCoefficients = CompareRaster,
                          NodeAreas = SubZoneSpatial, 
                          NodeOutlets = PointData, 
                          ReferenceConcentrations = ReferenceConcentrationData,
                          ComparisonConcentrations = ComparisonConcentrationData,
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
  
  #There are two map options for the export coefficient map, a raster version, and a chlopleth version based on the node areas.
  #The option plotted is determined by the class type of the input data

  RasterMap     <- FALSE
  ChlorplethMap <- FALSE
  if (class(ReferenceExportCoefficients) == "RasterLayer" & class(ComparisonExportCoefficients) == "RasterLayer") RasterMap <- TRUE else
    if (class(ReferenceExportCoefficients) == "data.frame" & class(ComparisonExportCoefficients) == "data.frame") ChlorplethMap <- TRUE 
  #else
  #  {exit with an error status explaining the inconsitency of the input data}
  
  if (RasterMap){
    #Calculate the perentage difference in the raster data
    DifferenceRaster <- min(50,max(-50,(ComparisonExportCoefficients - ReferenceExportCoefficients) / ReferenceExportCoefficients * 100))
    
    #Reformat the raster data for plotting in GGplot (see https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r)
    gplot_DifferenceRaster <- gplot_data(DifferenceRaster)
  }
  if (ChlorplethMap){
    ChangeInExportCoefficients <- merge(ReferenceExportCoefficients,ComparisonExportCoefficients,by="Node.Name") %>%
    #ChangeInExportCoefficients <- merge(ReferenceConcentrationData,ComparisonConcentrationData) %>%     #for testing  
      dplyr::rename(Reference = 2,Comparison = 3) %>%
      mutate(PercentChange = pmin(50,pmax(-50,(Comparison - Reference) / Reference * 100)))

    NodeAreas<- NodeAreas %>% merge(ChangeInExportCoefficients,by.x="Zone_Code",by.y="Node.Name")
  }
  
  #Calculate the percentage change in river concentrations
  ChangeInRiverConcentrations <- merge(ReferenceConcentrations,ComparisonConcentrations) %>% 
    dplyr::rename(Reference = 2,Comparison = 3) %>%
    mutate(PercentChange = pmin(50,pmax(-50,(Comparison - Reference) / Reference * 100)))

  #Add the scenario data to the zone spatial data
  NodeOutlets<- NodeOutlets %>% merge(ChangeInRiverConcentrations)
  
  theme_set(theme_map())
  MapExtent <- extent(NodeAreas)  
  
  #Create the export coefficient map
  ExportCoefficientChangeMap <- ggplot() +
    #Add a grey background
    geom_sf(data = NodeAreas, fill="#bcbcbc",size=0.06)+      
    #Set the position, size and title allignment of the legend
    theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5) +
    
    #Add either the Raster or Chlorpleth map for the change in export coefficients
    #+geom_tile(data = dplyr::filter(gplot_DifferenceRaster, !is.na(value)),aes(x=x,y=y, fill = value))+
    {if (RasterMap) {geom_tile(data = dplyr::filter(gplot_DifferenceRaster, !is.na(value)),aes(x=x,y=y, fill = value))}}+
    {if (ChlorplethMap) {geom_sf(data = NodeAreas, aes(fill = PercentChange))}}+
    # coord_sf(crs = "+init=epsg:2193")+
    
    #Add the node area boundaries, and the legend formatting details the coordinate system and extent
    scale_fill_gradientn(limits =c(-50,50),"Percentage \nchange in \nexport \ncoefficient",na.value = NA,
                         values = c(0,0.48,0.49,0.51,0.52,1),
                         #colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + #reversed colours
    colours=c("#2b83ba","#def2b4","#bcbcbc","#bcbcbc","#ffdf9a","#d7191c")) + 
    #Set the legend labels to be on the left
    guides(fill = guide_colourbar(label.position = "left")) +
    #add the node area outlines
    geom_sf(data = NodeAreas,fill = "transparent",size = 0.06) + 
    coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1],MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4]))
  
  
  
  RiverNutrientConcentrationChangeMap <- ggplot(data = NodeAreas) + 
    theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5)+
    geom_sf(fill = "#bcbcbc",size = 0.06)  + 
    geom_sf(data = NodeOutlets, aes(fill=PercentChange),col="black", size=3,pch=21)+
    scale_fill_gradientn(limits =c(-50,50),"Percentage \nchange in \nriver nutrient \nconcentration",na.value = NA,
                         values = c(0,0.48,0.49,0.51,0.52,1),
                         #colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + #Reversed colours
    colours=c("#2b83ba","#def2b4","#bcbcbc","#bcbcbc","#ffdf9a","#d7191c")) + 
    guides(fill = guide_colourbar(label.position = "left")) +
    north(SubZoneSpatial,"bottomleft",anchor = c(x =1727500, y = 5510000 ),scale = 0.15) +
    #scalebar(data = NodeAreas, dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
    #                          location = "bottomleft",st.dist=0.03,st.size=3) +
    scalebar(dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
             location = "bottomleft",st.dist=0.03,st.size=3,x.min=MapExtent[1]-30000,x.max=MapExtent[2],y.min=MapExtent[3],y.max=MapExtent[4]) +
    #coord_sf(crs = "+init=epsg:2193")
    #Set the coordinate system and extent
    #coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = MapExtent[3:4])
  coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4])) +
    labs(title = ImageName)
  
  DoubleMap <- ExportCoefficientChangeMap + RiverNutrientConcentrationChangeMap+ plot_layout(ncol = 2)
  
  #png(file.path(DiagramDirectory,"MapTest.png"),width = 160, height = 120, units = "mm",res = 300)
  #print(DoubleMap)
  
  #dev.off()
  
  return(DoubleMap)
}

#Function to map the export coefficients and river nutrient concentrations for a scenario 

AbsoluteMap <- function(ReferenceExportCoefficients = ReferenceRaster,
                          NodeAreas = SubZoneSpatial, 
                          NodeOutlets = PointData, 
                          ReferenceConcentrations = ReferenceConcentrationData,
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
  
  #There are two map options for the export coefficient map, a raster version, and a chlopleth version based on the node areas.
  #The option plotted is determined by the class type of the input data
  browser()
  RasterMap     <- FALSE
  ChlorplethMap <- FALSE
  if (class(ReferenceExportCoefficients) == "RasterLayer") RasterMap <- TRUE else
    if (class(ReferenceExportCoefficients) == "data.frame") ChlorplethMap <- TRUE 
  #else
  #  {exit with an error status explaining the inconsitency of the input data}
  
  if (RasterMap){
    #Reformat the raster data for plotting in GGplot (see https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r)
    gplot_AbsoluteRaster <- gplot_data(ReferenceExportCoefficients)
  }
  if (ChlorplethMap){
    AbsoluteExportCoefficients <- ReferenceExportCoefficients %>%
      #AbsoluteExportCoefficients <- merge(ReferenceConcentrationData,ComparisonConcentrationData) %>%     #for testing  
      dplyr::rename(Reference = 2)

    NodeAreas<- NodeAreas %>% merge(AbsoluteExportCoefficients,by.x="Zone_Code",by.y="Node.Name")
  }
  
  #Calculate the percentage change in river concentrations
  AbsoluteRiverConcentrations <- ReferenceConcentrations %>% 
    dplyr::rename(Reference = 2)
  
  #Add the scenario data to the zone spatial data
  NodeOutlets<- NodeOutlets %>% merge(AbsoluteRiverConcentrations)
  
  theme_set(theme_map())
  MapExtent <- extent(NodeAreas)  
  
  #Create the export coefficient map
  ExportCoefficientMap <- ggplot() +
    #Add a grey background
    geom_sf(data = NodeAreas, fill="#bcbcbc",size=0.06)+      
    #Set the position, size and title allignment of the legend
    theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5) +
    
    #Add either the Raster or Chlorpleth map for the change in export coefficients
    #+geom_tile(data = dplyr::filter(gplot_AbsoluteRaster, !is.na(value)),aes(x=x,y=y, fill = value))+
    {if (RasterMap) {geom_tile(data = dplyr::filter(gplot_AbsoluteRaster, !is.na(value)),aes(x=x,y=y, fill = value))}}+
    {if (ChlorplethMap) {geom_sf(data = NodeAreas, aes(fill = Reference))}}+
    # coord_sf(crs = "+init=epsg:2193")+
    
    #Add the node area boundaries, and the legend formatting details the coordinate system and extent
    scale_fill_gradientn(limits=c(0,30),"Export \ncoefficient \n(kg/ha/year)",na.value = NA,
                         values = c(0,1),
                         #colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + #reversed colours
                         colours=c("#ffdf9a","#d7191c")) + 
    #Set the legend labels to be on the left
    guides(fill = guide_colourbar(label.position = "left")) +
    #add the node area outlines
    geom_sf(data = NodeAreas,fill = "transparent",size = 0.06) + 
    coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1],MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4]))
  
  
  
  RiverNutrientConcentrationMap <- ggplot(data = NodeAreas) + 
    theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5)+
    geom_sf(fill = "#bcbcbc",size = 0.06)  + 
    geom_sf(data = NodeOutlets, aes(fill=Reference),col="black", size=3,pch=21)+
    scale_fill_gradientn(limits = c(0,15),"River nutrient \nconcentration \n(mg/litre)",na.value = NA,
                         values = c(0,1),
                         #colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + #Reversed colours
                         colours=c("#ffdf9a","#d7191c")) + 
    guides(fill = guide_colourbar(label.position = "left")) +
    north(SubZoneSpatial,"bottomleft",anchor = c(x =1727500, y = 5510000 ),scale = 0.15) +
    #scalebar(data = NodeAreas, dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
    #                          location = "bottomleft",st.dist=0.03,st.size=3) +
    scalebar(dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
             location = "bottomleft",st.dist=0.03,st.size=3,x.min=MapExtent[1]-30000,x.max=MapExtent[2],y.min=MapExtent[3],y.max=MapExtent[4]) +
    #coord_sf(crs = "+init=epsg:2193")
    #Set the coordinate system and extent
    #coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = MapExtent[3:4])
    coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4])) +
    labs(title = ImageName)
  
  DoubleMap <- ExportCoefficientMap + RiverNutrientConcentrationMap+ plot_layout(ncol = 2)
  
  #png(file.path(DiagramDirectory,"MapTest.png"),width = 160, height = 120, units = "mm",res = 300)
  #print(DoubleMap)
  
  #dev.off()
  
  return(DoubleMap)
}


#Set directories
ProjectDirectory <- "D:\\Projects\\LWP\\Horizons"
GISDirectory     <- file.path(ProjectDirectory,"Data\\GIS")
DataDirectory    <- file.path(ProjectDirectory, "Data")
DiagramDirectory <- file.path(ProjectDirectory,"Reports","Diagrams")

#Set file names

RiverConcentrationsDataFile   <- file.path(ProjectDirectory,"CASM","ScenarioOutcomeWMSZSummary.csv")

#Raster reference and comparison files
ReferenceFile                 <- file.path(GISDirectory,"MPILeachRateCoxCorrectedRaster 250x250.tif")
#ReferenceFile                <- file.path(GISDirectory,"ScenarioLeachRasters_IntensiveLanduseTable14-2.tif") #Scenario 1b

#ComparisonFile                <- file.path(GISDirectory,"ScenarioLeachRasters_IntensiveLanduseTable14-2.tif") #Scenario 1b
ComparisonFile                <- file.path(GISDirectory,"MPINativeLeachRateRaster 250x250.tif")               #Scenario 7 - Native land use
#ComparisonFile                <- file.path(GISDirectory,"ScenarioLeachRasters_IntensiveLanduseExistingTable14_2_Year20.tif") #Scenario 2b


#If comparing at the Water Management Subzone level comment/uncomment from these lines
{
#CASM Input files with the Water Management Sub-Zone Export Coeficients
BaselineFile <- file.path(DataDirectory,"CASM-Inputs_CoxCalLeach.xlsx") #This is the model baseline using Cox calibrated data
Scenario0_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2a_IntensiveOperativeTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0a" This is consented values or operative Table 14.2 year 5 for all intensive land use.
Scenario0_bFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2b_IntensiveOperativeTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0b" This is consented values or operative Table 14.2 year 20 for all intensive land use.
Scenario1_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3a_IntensiveTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1a" This is consented values or PC2 Table 14.2 year 5 for all intensive land use. 
Scenario1_bFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1b" This is consented values or PC2 Table 14.2 year 20 for all intensive land use. 
Scenario3_eFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5a_IntensiveTable14_2Year20OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 3e" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 20 for all intensive land use.
Scenario3_hFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5d_IntensiveTable14_2Year20OrConsentedOrDH18WithCoxCalibrated.xlsx") #Horizons "Scenario 3h" This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 20 for all intensive land use.
  
Scenario2_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4a_IntensiveTable14_2Year5OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 2a" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 5 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4b_IntensiveTable14_2Year5OrConsentedOrDH12WithCoxCalibrated.xlsx") #Horizons "Scenario 2b" This is consented values or Dave Horne reductions of 12 or PC2 Table 14.2 year 5 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4c_IntensiveTable14_2Year5OrConsentedOrDH15WithCoxCalibrated.xlsx") #Horizons "Scenario 2c" This is consented values or Dave Horne reductions of 15 or PC2 Table 14.2 year 5 for all intensive land use.
Scenario2_dFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4d_IntensiveTable14_2Year5OrConsentedOrDH18WithCoxCalibrated.xlsx") #Horizons "Scenario 2d" This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 5 for all intensive land use.

X4File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario4_IntensiveOperativeTable14_2Year20WithCoxCalibrated.xlsx")                          #Horizons "Scenario 4" Operative Table 14.2 everywhere
X5File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario5_11PctDairyExpanseOperativeTable14_2Year20WithCoxCalibrated.xlsx") #Horizons "Scenario 5" Dairy expansion with operative 14.2 everywhere
X6File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario6_IntensiveOverseerBaseRatesWithCoxCalibrated.xlsx")                                #Horizons "Scenario 6" Overseer base rates
X7File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario7_NaturalStateWithCoxCalibrated.xlsx")                                               #Horizons "Scenario 7" Native
X8File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario8_IntensiveTable14_2Year20OrConsentedOrFedFarmWithCoxCalibrated.xlsx")               #Horizons "Scenario 8, Fed. Farmers.
X9aFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario9a_PotatoOnLUC1to3DairyOrSheepAndBeef_CoxCalibrated.xlsx") #Horizons "Scenario 9a" Potatoes on LUC1 to 3 SHeep and Beef
X9bFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario9b_PotatoOnLUC1to3_CoxCalibrated.xlsx") #Horizons "Scenario 9b" Potatoes on LUC1 to 3
X10File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario10_LesserOfConsentedOrTable14.2_MPICoxCalibrated.xlsx") #Horizons "Scenario 10" lesser of consented or PC2 Table 14.2 Year 20 
TonFile <- file.path(DataDirectory,"CASM-Inputs_Tons_Scenario_2017PointSources_CoxCalibrated.xlsx") #Ton's Scenario, 2017 point sources 
}

SubZoneShapeFile    <- file.path(GISDirectory,"Water_Management_Subzones_cleaned","Water_Management_Subzones_cleaned.shp")
PointShapeFile      <- file.path(GISDirectory,"CASMOutputAsPoints","CASMOutputAsPoints.shp")

#Load shape files
SubZoneSpatial <- read_sf(SubZoneShapeFile)
PointData      <- read_sf(PointShapeFile)

#Load scenario rasters
ReferenceRaster <- raster(ReferenceFile)
CompareRaster <- raster(ComparisonFile)

#Load Scenario CASM Input data
#ReferenceData <-  WMSZMeanExportCoeffCalculator(CASMInputFile = ReferenceDataFile)
#ComparisonData <- WMSZMeanExportCoeffCalculator(CASMInputFile = ComparisonDataFile)

#Load scenario output data
ScenarioData <- read.csv(RiverConcentrationsDataFile)

ScenarioColumns <- c("Baseline",	"Scenario0_a",	"Scenario0_b",	"Scenario1_a",	"Scenario1_b"	,"Scenario2_a",	"Scenario2_b",	"Scenario2_c",	"Scenario2_d",	"Scenario3_e",	"Scenario3_f","Scenario3_g",	"Scenario3_h"	,		"X4",	"X5",	"X6",	"X7",	"X8",	"X9a",	"X9b",	"X10",	"Ton")
#ReferenceScenarioColumn <- "Baseline"
#ComparisonScenarioColumn <- "X4"


PlotGenerator <- function(Comparison = ComparisonScenarioColumn,Reference = ReferenceScenarioColumn){

  #Figure out the files to load
  ReferenceExportCoefficientsFileNamereference <- paste0(Reference,"File")
  ComparisonExportCoefficientsFileNamereference <- paste0(Comparison,"File")
  
  #Load Scenario CASM Input data
  ReferenceData <-  WMSZMeanExportCoeffCalculator(CASMInputFile = get(ReferenceExportCoefficientsFileNamereference))
  ComparisonData <- WMSZMeanExportCoeffCalculator(CASMInputFile = get(ComparisonExportCoefficientsFileNamereference))  
  
#Select the nutrient concentration data of interest
ReferenceConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Reference)  #Horizons "Baseline_Scenario 0b"This is consented values or operative Table 14.2 year 20 for all intensive land use.
ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Comparison) #Horizons "Scenario 1b". This is consented values or PC2 Table 14.2 year 20 for all intensive land use."

OutputFileName <- paste0(Comparison,"vs",Reference,".png")
  
OutMap <- DifferenceMap(ReferenceExportCoefficients = ReferenceData,
                        ComparisonExportCoefficients = ComparisonData,
                        ReferenceConcentrations = ReferenceConcentrationData,
                        ComparisonConcentrations = ComparisonConcentrationData,
                        ImageName = OutputFileName)

#OutMap <- DifferenceMap(BaselineExportCoefficients = ReferenceRaster,ComparisonExportCoefficients = CompareRaster)

png(file.path(DiagramDirectory,OutputFileName),width = 160, height = 120, units = "mm",res = 300)
print(OutMap)

dev.off()
}

AbsolutePlotGenerator <- function(Reference = ReferenceScenarioColumn){
  
  #Figure out the files to load
  ReferenceExportCoefficientsFileNamereference <- paste0(Reference,"File")
  
  #Load Scenario CASM Input data
  ReferenceData <-  WMSZMeanExportCoeffCalculator(CASMInputFile = get(ReferenceExportCoefficientsFileNamereference))
  
  #Select the nutrient concentration data of interest
  ReferenceConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Reference)  #Horizons "Baseline_Scenario 0b"This is consented values or operative Table 14.2 year 20 for all intensive land use.

  OutputFileName <- paste0(Reference,".png")
  
  OutMap <- AbsoluteMap(ReferenceExportCoefficients = ReferenceData,
                          ReferenceConcentrations = ReferenceConcentrationData,
                          ImageName = OutputFileName)
  
  #OutMap <- DifferenceMap(BaselineExportCoefficients = ReferenceRaster,ComparisonExportCoefficients = CompareRaster)
  
  png(file.path(DiagramDirectory,OutputFileName),width = 160, height = 120, units = "mm",res = 300)
  print(OutMap)
  
  dev.off()
}

  
# PlotGenerator(Comparison = "X4",Reference = "Baseline")             #Operative, no consents, operative vs model baseline
# PlotGenerator(Comparison = "Scenario0_b",Reference = "X4")           #Operative, consents, operative vs #Operative, no consents
# PlotGenerator(Comparison = "X5",Reference = "X4")                    #Opertive with dairy expansion vs operative
# 
# PlotGenerator(Comparison = "Scenario1_b",Reference = "Scenario0_b")  #
# PlotGenerator(Comparison = "Scenario3_e",Reference = "Scenario1_b")
# PlotGenerator(Comparison = "Scenario3_h",Reference = "Scenario3_e")
# PlotGenerator(Comparison = "Scenario3_h",Reference = "Scenario1_b")
# PlotGenerator(Comparison = "Scenario2_a",Reference = "Scenario3_e")
# PlotGenerator(Comparison = "Scenario2_d",Reference = "Scenario3_h")
# PlotGenerator(Comparison = "X10",Reference = "Scenario1_b")
# 
# 
# PlotGenerator(Comparison = "X7",Reference = "Baseline")
# PlotGenerator(Comparison = "X8",Reference = "Scenario1_b")
# PlotGenerator(Comparison = "X9a",Reference = "Baseline")
# PlotGenerator(Comparison = "X9b",Reference = "Baseline")
# PlotGenerator(Comparison = "Ton",Reference = "Baseline")

#AbsolutePlotGenerator(Reference = "Baseline")
