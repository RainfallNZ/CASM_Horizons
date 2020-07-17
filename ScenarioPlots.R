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



#' Function to get the mean export coefficient fr each water management sub zone from the "Diffuse Inputs" sheet in a CASM Input spreadsheet
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

DifferenceMap <- function(BaselineExportCoefficients = ReferenceRaster,
                          ComparisonExportCoefficients = CompareRaster,
                          NodeAreas = SubZoneSpatial, 
                          NodeOutlets = PointData, 
                          BaselineConcentrations = BaselineConcentrationData,
                          ComparisonConcentrations = ComparisonConcentrationData){
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
  if (class(BaselineExportCoefficients) == "RasterLayer" & class(ComparisonExportCoefficients) == "RasterLayer") RasterMap <- TRUE else
    if (class(BaselineExportCoefficients) == "data.frame" & class(ComparisonExportCoefficients) == "data.frame") ChlorplethMap <- TRUE 
  #else
  #  {exit with an error status explaining the inconsitency of the input data}
  
  if (RasterMap){
    #Calculate the perentage difference in the raster data
    DifferenceRaster <- min(50,max(-50,(ComparisonExportCoefficients - BaselineExportCoefficients) / BaselineExportCoefficients * 100))
    
    #Reformat the raster data for plotting in GGplot (see https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r)
    gplot_DifferenceRaster <- gplot_data(DifferenceRaster)
  }
  if (ChlorplethMap){
    ChangeInExportCoefficients <- merge(BaselineExportCoefficients,ComparisonExportCoefficients,by="Node.Name") %>%
    #ChangeInExportCoefficients <- merge(BaselineConcentrationData,ComparisonConcentrationData) %>%     #for testing  
      dplyr::rename(Baseline = 2,Comparison = 3) %>%
      mutate(PercentChange = pmin(50,pmax(-50,(Comparison - Baseline) / Baseline * 100)))

    NodeAreas<- NodeAreas %>% merge(ChangeInExportCoefficients,by.x="Zone_Code",by.y="Node.Name")
  }
  
  #Calculate the percentage change in river concentrations
  ChangeInRiverConcentrations <- merge(BaselineConcentrationData,ComparisonConcentrationData) %>% 
    dplyr::rename(Baseline = 2,Comparison = 3) %>%
    mutate(PercentChange = pmin(50,pmax(-50,(Comparison - Baseline) / Baseline * 100)))

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
                         colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) +
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
                         colours=c("#d7191c","#ffdf9a","#bcbcbc","#bcbcbc","#def2b4","#2b83ba")) + 
    guides(fill = guide_colourbar(label.position = "left")) +
    north(SubZoneSpatial,"bottomleft",anchor = c(x =1727500, y = 5510000 ),scale = 0.15) +
    #scalebar(data = NodeAreas, dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
    #                          location = "bottomleft",st.dist=0.03,st.size=3) +
    scalebar(dist = 25, dist_unit = "km", transform = FALSE, st.bottom = FALSE,
             location = "bottomleft",st.dist=0.03,st.size=3,x.min=MapExtent[1]-30000,x.max=MapExtent[2],y.min=MapExtent[3],y.max=MapExtent[4]) +
    #coord_sf(crs = "+init=epsg:2193")
    #Set the coordinate system and extent
    #coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = MapExtent[3:4])
  coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = c(MapExtent[3],MapExtent[4]))
  
  DoubleMap <- ExportCoefficientChangeMap + RiverNutrientConcentrationChangeMap+ plot_layout(ncol = 2)
  
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
ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_CoxCalLeach.xlsx") #This is the model baseline using Cox calibrated data
#ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2a_IntensiveOperativeTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0a" This is consented values or operative Table 14.2 year 5 for all intensive land use.
#ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2b_IntensiveOperativeTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0b" This is consented values or operative Table 14.2 year 20 for all intensive land use.
#ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3a_IntensiveTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1a" This is consented values or PC2 Table 14.2 year 5 for all intensive land use. 
#ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1b" This is consented values or PC2 Table 14.2 year 20 for all intensive land use. 
#ReferenceDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario4_IntensiveOperativeTable14_2Year20WithCoxCalibrated.xlsx")                          #Horizons "Scenario 4" Operative Table 14.2 everywhere


#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2b_IntensiveOperativeTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0b" This is consented values or operative Table 14.2 year 20 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 1b" This is consented values or PC2 Table 14.2 year 20 for all intensive land use.

#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4a_IntensiveTable14_2Year5OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 2a" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 5 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4b_IntensiveTable14_2Year5OrConsentedOrDH12WithCoxCalibrated.xlsx") #Horizons "Scenario 2b" This is consented values or Dave Horne reductions of 12 or PC2 Table 14.2 year 5 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4c_IntensiveTable14_2Year5OrConsentedOrDH15WithCoxCalibrated.xlsx") #Horizons "Scenario 2c" This is consented values or Dave Horne reductions of 15 or PC2 Table 14.2 year 5 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4d_IntensiveTable14_2Year5OrConsentedOrDH18WithCoxCalibrated.xlsx") #Horizons "Scenario 2d" This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 5 for all intensive land use.

#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5a_IntensiveTable14_2Year5OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 3a" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 20 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5b_IntensiveTable14_2Year5OrConsentedOrDH12WithCoxCalibrated.xlsx") #Horizons "Scenario 3b" This is consented values or Dave Horne reductions of 12 or PC2 Table 14.2 year 20 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5c_IntensiveTable14_2Year5OrConsentedOrDH15WithCoxCalibrated.xlsx") #Horizons "Scenario 3c" This is consented values or Dave Horne reductions of 15 or PC2 Table 14.2 year 20 for all intensive land use.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5d_IntensiveTable14_2Year5OrConsentedOrDH18WithCoxCalibrated.xlsx") #Horizons "Scenario 3d" This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 20 for all intensive land use.

#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario4_IntensiveOperativeTable14_2Year20WithCoxCalibrated.xlsx")                          #Horizons "Scenario 4" Operative Table 14.2 everywhere
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario5_11PctDairyExpanseOperativeTable14_2Year20WithCoxCalibrated.xlsx") #Horizons "Scenario 5" Dairy expansion with operative 14.2 everywhere
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario6_IntensiveOverseerBaseRatesWithCoxCalibrated.xlsx")                                #Horizons "Scenario 6" Overseer base rates
ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario7_NaturalStateWithCoxCalibrated.xlsx")                                               #Horizons "Scenario 7" Native
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario8_IntensiveTable14_2Year20OrConsentedOrFedFarmWithCoxCalibrated.xlsx")               #Horizons "Scenario 8, Fed. Farmers.
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario9a_PotatoOnLUC1to3DairyOrSheepAndBeef_CoxCalibrated.xlsx") #Horizons "Scenario 9a" Potatoes on LUC1 to 3 SHeep and Beef
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario9b_PotatoOnLUC1to3_CoxCalibrated") #Horizons "Scenario 9b" Potatoes on LUC1 to 3
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario10_LesserOfConsentedOrTable14.2_MPICoxCalibrated.xlsx") #Horizons "Scenario 10" lesser of consented or PC2 Table 14.2 Year 20 
#ComparisonDataFile <- file.path(DataDirectory,"CASM-Inputs_Tons_Scenario_2017PointSources_CoxCalibrated.xlsx") #Ton's Scenario, 2017 point sources 
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
ReferenceData <-  WMSZMeanExportCoeffCalculator(CASMInputFile = ReferenceDataFile)
ComparisonData <- WMSZMeanExportCoeffCalculator(CASMInputFile = ComparisonDataFile)

#Load scenario output data
ScenarioData <- read.csv(RiverConcentrationsDataFile)

#Select the nutrient concentration data of interest
#BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Current) #This is the model baseline from the Cox Calibrated data
#BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Baseline_a)  #Horizons "Baseline_Scenario 0a"This is consented values or operative Table 14.2 year 5 for all intensive land use. 
BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Baseline_b)  #Horizons "Baseline_Scenario 0b"This is consented values or operative Table 14.2 year 20 for all intensive land use.
#BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario1_a) #Horizons "Scenario 1a" This is consented values or PC2 Table 14.2 year 5 for all intensive land use. 
#BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario1_b) #Horizons "Scenario 1b" This is consented values or PC2 Table 14.2 year 20 for all intensive land use. 


#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Baseline_b)  #Horizons "Baseline_Scenario 0b"This is consented values or operative Table 14.2 year 20 for all intensive land use. 
ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario1_b) #Horizons "Scenario 1b". This is consented values or PC2 Table 14.2 year 20 for all intensive land use."

#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario2_a) #Horizons "Scenario 2a". This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 5 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario2_b) #Horizons "Scenario 2b". This is consented values or Dave Horne reductions of 12 or PC2 Table 14.2 year 5 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario2_c) #Horizons "Scenario 2c". This is consented values or Dave Horne reductions of 15 or PC2 Table 14.2 year 5 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario2_d) #Horizons "Scenario 2d". This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 5 for all intensive land use."

#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario3_a) #Horizons "Scenario 3e". This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 20 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario3_b) #Horizons "Scenario 3f". This is consented values or Dave Horne reductions of 12 or PC2 Table 14.2 year 20 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario3_c) #Horizons "Scenario 3g". This is consented values or Dave Horne reductions of 15 or PC2 Table 14.2 year 20 for all intensive land use."
#ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario3_d) #Horizons "Scenario 3h". This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 20 for all intensive land use."


OutMap <- DifferenceMap(BaselineExportCoefficients = ReferenceData,ComparisonExportCoefficients = ComparisonData)
#OutMap <- DifferenceMap(BaselineExportCoefficients = ReferenceRaster,ComparisonExportCoefficients = CompareRaster)
png(file.path(DiagramDirectory,"MapTest.png"),width = 160, height = 120, units = "mm",res = 300)
print(OutMap)

dev.off()
  
  