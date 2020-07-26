#Script to tabulate diferences between scenario data and output
#The intention is to provide tables of summaries of the changes in export coefficients and river nutrient concentrations.
#The river concentrations come from the scenario output summary data compiled by Ton from Tim Cox's output.
#The export coefficient data comes from either the CASM input data, combined with the Water Management sub zone spatial data.
#The reference SIN Target data come from a spreadsheet previously prepared by Caroline Fraser

#The summary statistics of interest are:
#1/ Total number of Water Management Sub Zones with scenario export coefficients higher than the comparator
#2/ % higher
#3/ Average value
#4/ Average value for those that are higher
#5/ Total number lower
#6/ % lower

#7:12/ Repeat of the above for the river concentrations

#13:18/ Repeat of above for the scenario soluble inorganic nitrogen concentrations with respect to the the target
#Some functions
#' Function to get the mean export coefficient for each water management sub zone from the "Diffuse Inputs" sheet in a CASM Input spreadsheet
#' @param CASMInputFile The CASM input spreadsheet filename
#' @return A data frame with two columns, The first called "Node.Name", the second with the export coefficients in it (rounded to 1 decimal place)
#' @export
WMSZMeanExportCoeffCalculator <- function(CASMInputFile = ReferenceDataFile){
  
  InputData <- read.xlsx(CASMInputFile,sheet = "Diffuse Inputs")[,c("Node.Name","Land.Area.(ha)","Export.Coeff.(kg/ha/yr)")]
  InputData$Node.Name <- sub("-.*$","",InputData$Node.Name)
  InputData$Load <- InputData$`Export.Coeff.(kg/ha/yr)` * InputData$`Land.Area.(ha)`
  AggregatedData <- aggregate(InputData[,c("Load","Land.Area.(ha)")],list(InputData$Node.Name),sum)
  AggregatedData$`Export.Coeff.(kg/ha/yr)` <- round(AggregatedData$Load / AggregatedData$`Land.Area.(ha)`,1)
  AggregatedData <- dplyr::rename(AggregatedData, Node.Name = 1) %>% dplyr::select("Node.Name","Export.Coeff.(kg/ha/yr)")
  return(AggregatedData)
}


#load libraries
if (!require(rgdal)) install.packages("rgdal"); library(rgdal)   
if (!require(raster)) install.packages("raster"); library(raster)
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(sf)) install.packages("sf"); library(sf)
if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)                #needed to load excel data
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)              

#Set directories
#Set directories
ProjectDirectory <- "D:\\Projects\\LWP\\Horizons"
GISDirectory     <- file.path(ProjectDirectory,"Data\\GIS")
DataDirectory    <- file.path(ProjectDirectory, "Data")
CASMDirectory    <- file.path(ProjectDirectory,"CASM")
DiagramDirectory <- file.path(ProjectDirectory,"Reports","Diagrams")

#Set file names
SINDataFile                   <- file.path(DataDirectory,"AssessmentPointCriteriaAndConcentrations.xlsx")
RiverConcentrationsDataFile   <- file.path(ProjectDirectory,"CASM","ScenarioOutcomeWMSZSummary.csv")

#Load the SIN data
SINData                       <- read.xlsx(SINDataFile,sheet = "AssessmentPointCriteriaAndConce")

#Load the river concentration data
RiverConcData <- read.csv(RiverConcentrationsDataFile)

#CASM Input files with the Water Management Sub-Zone Export Coeficients
BaselineFile <- file.path(DataDirectory,"CASM-Inputs_CoxCalLeach.xlsx") #This is the model baseline using Cox calibrated data
Scenario0_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2a_IntensiveOperativeTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0a" This is consented values or operative Table 14.2 year 5 for all intensive land use.
Scenario0_bFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_2b_IntensiveOperativeTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Baseline_Scenario 0b" This is consented values or operative Table 14.2 year 20 for all intensive land use.
Scenario1_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3a_IntensiveTable14_2Year5OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1a" This is consented values or PC2 Table 14.2 year 5 for all intensive land use. 
Scenario1_bFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx") #Horizons "Scenario 1b" This is consented values or PC2 Table 14.2 year 20 for all intensive land use. 
Scenario3_eFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5a_IntensiveTable14_2Year20OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 3e" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 20 for all intensive land use.
Scenario3_hFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_5d_IntensiveTable14_2Year20OrConsentedOrDH18WithCoxCalibrated.xlsx") #Horizons "Scenario 3h" This is consented values or Dave Horne reductions of 18 or PC2 Table 14.2 year 20 for all intensive land use.

Scenario2_aFile <- file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_4a_IntensiveTable14_2Year5OrConsentedOrDH9WithCoxCalibrated.xlsx") #Horizons "Scenario 2a" This is consented values or Dave Horne reductions of 9 or PC2 Table 14.2 year 5 for all intensive land use.
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
X12File <- file.path(DataDirectory,"CASM-Inputs_Horizons_Scenario12_ConsentedOrBaseOrCoxCalibrated.xlsx") #Horizons "Scenario 12" 


#Start by creating a data frame of all the input data with the rowname being the WMSZ and the column being the scenario
Scenarios <- c("Baseline","Scenario0_a" ,"Scenario0_b", "Scenario1_a", "Scenario1_b", "Scenario2_a",
               "Scenario2_d" ,"Scenario3_e", "Scenario3_h", "X4","X5","X6","X7","X8","X9a","X9b","X10","Ton","X12")

ListOfScenarioExportCoefficeints <-  lapply(Scenarios, function(Scenario){
  ScenarioFilenameReference <- paste0(Scenario,"File")
  ScenarioExportCoefficeints <- WMSZMeanExportCoeffCalculator(CASMInputFile = get(ScenarioFilenameReference))
  return(ScenarioExportCoefficeints)
})
  
ScenarioExportCoefficients <- ListOfScenarioExportCoefficeints %>% reduce(left_join, by = "Node.Name")
names(ScenarioExportCoefficients)[-1] <- Scenarios


#Build up a scenario pairing data frame which gives the scenario being compared, and the reference scenario that it is being compared too
ScenarioPairs <- data.frame(Comparison = "X4",Reference = "Baseline", stringsAsFactors = FALSE)
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario0_b",Reference = "X4", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X5",Reference = "X4", stringsAsFactors = FALSE))                    
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X6",Reference = "Baseline", stringsAsFactors = FALSE))                    
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario1_b",Reference = "Scenario0_b", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario3_e",Reference = "Scenario1_b", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario3_h",Reference = "Scenario3_e", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario3_h",Reference = "Scenario1_b", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario2_a",Reference = "Scenario3_e", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Scenario2_d",Reference = "Scenario3_h", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X10",Reference = "Scenario1_b", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X7",Reference = "Baseline", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X8",Reference = "Scenario1_b", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X9a",Reference = "Baseline", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X9b",Reference = "Baseline", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "Ton",Reference = "Baseline", stringsAsFactors = FALSE))
ScenarioPairs <- rbind(ScenarioPairs,data.frame(Comparison = "X12",Reference = "X6", stringsAsFactors = FALSE))

#Now work through the Scenario-Comparator pairs and build up the summary statistics
ListOfComparisonSummaryStatistics <- apply(ScenarioPairs,1, function(ScenarioComparison) { #ScenarioComparison <- ScenarioPairs[1,]
  #Start with the Export Coefficient Data
  #browser()
  ScenarioName <- ScenarioComparison["Comparison"]
  ComparatorName <- ScenarioComparison["Reference"]
  WhichECHigher <- which(ScenarioExportCoefficients[,ScenarioName] > ScenarioExportCoefficients[,ComparatorName])
  NumberECHigher <- length(WhichECHigher)
  PercentECHigher <- NumberECHigher / nrow(ScenarioExportCoefficients)
  AverageECHigher <- mean(ScenarioExportCoefficients[WhichECHigher,ScenarioName])
  NumberECLower <- sum(ScenarioExportCoefficients[,ScenarioName] < ScenarioExportCoefficients[,ComparatorName])
  PercentECLower <- NumberECLower / nrow(ScenarioExportCoefficients)
  
  #Then do the river concentration  data
  WhichRCHigher <- which(RiverConcData[,ScenarioName] > RiverConcData[,ComparatorName])
  NumberRCHigher <- length(WhichRCHigher)
  PercentRCHigher <- NumberRCHigher / nrow(RiverConcData)
  AverageRCHigher <- mean(RiverConcData[WhichRCHigher,ScenarioName])
  NumberRCLower <- sum(RiverConcData[,ScenarioName] < RiverConcData[,ComparatorName])
  PercentRCLower <- NumberRCLower / nrow(RiverConcData)
  
  Summary <- data.frame(Scenario = ScenarioName,
                        Comparator = ComparatorName,
                        No.ECHigh = NumberECHigher,
                        PctECHigh = PercentECHigher,
                        AveECHigh = AverageECHigher,
                        No.EClow = NumberECLower,
                        PctECLow = PercentECLower,
                        No.RCHigh = NumberRCHigher,
                        PctRCHigh = PercentRCHigher,
                        AveRCHigh = AverageRCHigher,
                        No.RClow = NumberRCLower,
                        PctRCLow = PercentRCLower
                        )
  return(Summary)
})
  
  
SummaryStatistics <- do.call("rbind",ListOfComparisonSummaryStatistics)  
  
  
  
