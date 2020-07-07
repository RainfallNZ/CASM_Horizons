#Script to map diffeences between sceario data and output
#The intention is to provide side-by-side maps of the change in export coefficients (left map) and the change in river nutrient concentrations (right map)
#The river concentrations come from the scenario output summary data compiled by Ton from TIm Cox's output.
#The export coefficient data comes from either the CASM input data, combined with teh Water Management sub zone spatial data, or, 
#if available for both scenarios, the raster difference in leach rates.
#load libraries
if (!require(rgdal)) install.packages("rgdal"); library(rgdal)   
if (!require(raster)) install.packages("raster"); library(raster)
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)

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

#Set directories
ProjectDirectory <- "D:\\Projects\\LWP\\Horizons"
GISDirectory     <- file.path(ProjectDirectory,"Data\\GIS")
DataDirectory    <- file.path(ProjectDirectory, "Data")
DiagramDirectory <- file.path(ProjectDirectory,"Reports","Diagrams")

#Set file names
RiverConcentrationsDataFile   <- file.path(ProjectDirectory,"CASM","ScenarioOutcomeWMSZSummary.csv")

MPICoxCorrectedLeachRatesFile <- file.path(GISDirectory,"MPILeachRateCoxCorrectedRaster 250x250.tif")
ComparisonFile                <- file.path(GISDirectory,"ScenarioLeachRasters_IntensiveLanduseTable14-2.tif")

SubZoneShapeFile    <- file.path(GISDirectory,"Water_Management_Subzones_cleaned","Water_Management_Subzones_cleaned.shp")
PointShapeFile      <- file.path(GISDirectory,"CASMOutputAsPoints","CASMOutputAsPoints.shp")

#Load shape files
SubZoneSpatial <- read_sf(SubZoneShapeFile)
PointData      <- read_sf(PointShapeFile)

#Load rasters
BaseScenario <- raster(MPICoxCorrectedLeachRatesFile)
CompareRaster <- raster(ComparisonFile)

#Load scenario data
ScenarioData <- read.csv(RiverConcentrationsDataFile)

#Select the nutrient concentration data of interest
BaselineConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Baseline_a)
ComparisonConcentrationData <- ScenarioData %>% dplyr::select(WMSZ,Scenario3_d)

#Function to map difference between rasters 

DifferenceMap <- function(BaselineExportCoefficients = BaseScenario,
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
    if (class(BaselineExportCoefficients) == "RasterLayer" & class(ComparisonExportCoefficients) == "RasterLayer") Chlopleth <- TRUE 
  #else
  #  {exit with an error status explaining the inconsitency of the input data}
  
  if (RasterMap){
  #Calculate the perentage difference in the raster data
  DifferenceRaster <- (ComparisonExportCoefficients - BaselineExportCoefficients) #/ BaselineExportCoefficients * 100
  
  #Reformat the raster data for plotting in GGplot (see https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r)
  gplot_DifferenceRaster <- gplot_data(DifferenceRaster)
  }
  if (ChlorplethMap){
    #ChangeInExportCoefficients <- merge(BaselineExportCoefficients,BaselineExportCoefficients) %>%
      ChangeInExportCoefficients <- merge(BaselineConcentrationData,ComparisonConcentrationData) %>%     #for testing  
      rename(Baseline = 2,Comparison = 3) %>%
      mutate(PercentChange = (Comparison - Baseline) / Baseline * 100)
      NodeAreas<- NodeAreas %>% merge(ChangeInRiverConcentrations,by.x="Zone_Code",by.y="WMSZ")
  }
  
  #Calculate the percentage change in river concentrations
  ChangeInRiverConcentrations <- merge(BaselineConcentrationData,ComparisonConcentrationData) %>% 
    rename(Baseline = 2,Comparison = 3) %>%
    mutate(PercentChange = (Comparison - Baseline) / Baseline * 100)
  
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
    geom_sf(data = NodeAreas,fill = "transparent",size = 0.06)

  

  RiverNutrientConcentrationChangeMap <- ggplot(data = NodeAreas) + 
    theme(legend.position = c(0.78,0.55),legend.key.height = unit(0.6,"cm"),legend.title.align=0.5)+
    geom_sf(fill = "#bcbcbc",size = 0.06)  + 
    geom_sf(data = NodeOutlets, aes(fill=Change_1b),col="black", size=3,pch=21)+
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
  coord_sf(crs = "+init=epsg:2193",xlim = c(MapExtent[1]-30000,MapExtent[2]),ylim = MapExtent[3:4])
  
  DoubleMap <- ExportCoefficientChangeMap + RiverNutrientConcentrationChangeMap+ plot_layout(ncol = 2)

  png(file.path(DiagramDirectory,"MapTest.png"),width = 160, height = 120, units = "mm",res = 300)
  print(DoubleMap)

  dev.off()

  return(DoubleMap)
}


  
  