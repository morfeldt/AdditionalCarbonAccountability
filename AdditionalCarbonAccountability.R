# AdditionalCarbonAccountability.R
# Code used for the academic article Hahn et al. 'Estimating countries´ carbon accountability for closing the mitigation gap based on past and future emissions'.
# The code reads data from the Global Carbon Project, the World Bank, and the UN Population Division, and calculates the annual emissions for each country from 2023 to 2070. It then calculates the global emission curves for 1.5 and 2 degrees Celsius, and the additional carbon accountability for each country based on different allocation principles. The code also creates figures for illustrating the results and a figure showing the planned emissions for selected countries.

# Load necessary libraries
library(xlsx)
library(reshape2)
library(ggplot2)
library(ggh4x)
library(wbstats)
library(jsonlite)
library(httr)
library(doParallel)
library(ggrepel)
library(ggforce)
library(scico)

#### INITIALIZATION ####
# Empty the workspace and memory
rm(list = ls(all.names = TRUE))
gc()

# Set some parameters
NoCores <- detectCores() - 1 # Set the number of cores to use for parallel processing
CarbonBudget = data.frame(TempTarget = c(1.5,2),
                          BudgetGtCO2 = c(250, 800)-25) # Set the carbon budget for 1.5 and 2 degrees
CountryAssumptions <- subset(read.xlsx("CountryAssumptions.xlsx", sheetIndex = 1, colIndex = 1:6)) # Read assumptions for countries including Sweden

#### PREPARATION OF DATA ######
# Read data from Global Carbon Project and WorldBank
DataWorldBank <- wb_data(c(GDP = "NY.GDP.MKTP.CD", GDPpc = "NY.GDP.PCAP.CD"), country = "all", start_date = 1960, end_date = 2022) # Read GDP and GDP per capita data from WorldBank
# Rename variables and countries
colnames(DataWorldBank)[which(names(DataWorldBank) == "date")] <- "Year" 
colnames(DataWorldBank)[which(names(DataWorldBank) == "country")] <- "Country"
DataWorldBank$Country[DataWorldBank$iso3c == "EGY"] <- "Egypt"
DataWorldBank$Country[DataWorldBank$iso3c == "GMB"] <- "Gambia"
DataWorldBank$Country[DataWorldBank$iso3c == "IRN"] <- "Iran"
DataWorldBank$Country[DataWorldBank$iso3c == "RUS"] <- "Russia"
DataWorldBank$Country[DataWorldBank$iso3c == "KOR"] <- "South Korea"
DataWorldBank$Country[DataWorldBank$iso3c == "TUR"] <- "Türkiye"

# Add iso classification for countries from 'DataWorldBank' to 'CountryAssumptions'
CountryAssumptions <- merge(CountryAssumptions, subset(DataWorldBank, Country %in% CountryAssumptions$Country & Year == 2022, select = c(Country,iso3c)), by = "Country", all.x = TRUE)

# Download and Read data from Global Carbon Project
GCBDataFile <- tempfile(fileext = ".xlsx")
download.file("https://globalcarbonbudgetdata.org/downloads/latest-data/National_Fossil_Carbon_Emissions_2023v1.0.xlsx", destfile = GCBDataFile, mode = "wb")
DataGlobalCarbonBudget <- melt(merge(melt(data = read.xlsx(GCBDataFile, sheetName = "Territorial Emissions", startRow = 12), id = "NA.", value.name = "Territorial Emissions", variable.name = "Country"),
                                     melt(data = read.xlsx(GCBDataFile, sheetName = "Consumption Emissions", startRow = 9), id = "NA.", value.name = "Consumption Emissions", variable.name = "Country"), by = c("Country", "NA."), all.x = TRUE), id = c("NA.", "Country"), value.name = "EmissionsMtCO2", variable.name = "Accounting")
# Rename variables and countries
colnames(DataGlobalCarbonBudget)[which(names(DataGlobalCarbonBudget) == "NA.")] <- "Year" # Rename column for year from 'NA.' to 'Year'
DataGlobalCarbonBudget$EmissionsMtCO2 <- DataGlobalCarbonBudget$EmissionsMtCO2 * 44/12 # Convert emission data from MtC to MtCO2
DataGlobalCarbonBudget$Country <- as.character(DataGlobalCarbonBudget$Country) # Convert 'Country' to character
DataGlobalCarbonBudget$Country <- gsub(".", " ", DataGlobalCarbonBudget$Country, fixed = TRUE) # Replace '.' with ' ' in 'Country'
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "USA"] <- "United States" # Replace 'USA' with 'United States' in 'Country'
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "EU27"] <- "European Union" # Replace 'EU27' with 'European Union' in 'Country
DataGlobalCarbonBudget <- merge(DataGlobalCarbonBudget, subset(CountryAssumptions, select = c(Country, iso3c)), by = "Country") # Add iso classification for countries to 'DataGlobalCarbonBudget'
DataGlobalCarbonBudgetALL <- DataGlobalCarbonBudget

# Define function to call the UN API
callAPI <- function(relative_path, topics_list=FALSE){
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, relative_path)
  response <- fromJSON(target)
  # Checks if response was a flat file or a list (indicating pagination)
  # If response is a list, we may need to loop through the pages to get all of the data
  if (class(response)=="list"){
    # Create a dataframe from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)){
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)}
  # Otherwise, we will simply load the data directly from the API into a dataframe
  else{
    if (topics_list==TRUE){
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    }
    else{
      df <- fromJSON(target)        
      return(df)
    }
  }
}
UNLocations <- callAPI("/locations/")

# Download and read data from the UN Population Division
EUStatesISO2 <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI") # Define the ISO2 codes for the EU states (excluding Sweden)
# Download data from the UN Population Division (not used due to long response times)
#DataUNPopulation <- 
#  foreach(i = UNLocations$id[UNLocations$iso3 %in% CountryAssumptions$iso3c | UNLocations$iso2 %in% EUStatesISO2], .combine = "rbind") %do% {
#    subset(callAPI(paste0("/data/indicators/49/locations/",i,"/start/1960/end/2070")), sexId == 3 & variantId == 4, select = c(location, iso3, iso2, timeLabel, value))
#  }
#https://population.un.org/dataportal/data/indicators/49/locations/32,36,40,56,76,100,124,152,156,170,188,191,196,203,208,231,233,246,250,270,276,348,356,360,364,372,380,392,398,404,410,428,440,442,470,484,504,528,554,566,578,604,608,616,620,642,643,682,702,703,704,705,710,724,752,756,764,784,792,818,826,840,900/start/1960/end/2070/table/pivotbylocation?df=1f046dc3-8d29-4199-b85a-b024caa951e3

# Read data from the UN Population Division file instead of downloading (due to long response times in the UN Population API)
DataUNPopulation <- subset(read.csv("unpopulation_dataportal.csv"), SexId == 3 & VariantId == 4, select = c(Location, Iso3, Iso2, Time, Value)) # Read Population data from the UN Population Division

DataUNPopulation <- rbind(DataUNPopulation, cbind(data.frame(Location = rep("European Union", length(1960:2070)),
                                   Iso3 = rep("EUU", length(1960:2070)),
                                   Iso2 = rep("EU", length(1960:2070)),
                                   aggregate(Value ~ Time, subset(DataUNPopulation, Iso2 %in% EUStatesISO2),sum)))) # Aggregate the population data for the EU states
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Location")] <- "Country" # Rename 'Location' to 'Country'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Time")] <- "Year" # Rename 'Time' to 'Year'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Value")] <- "Population" # Rename 'Value' to 'Population'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso2")] <- "iso2c" # Rename 'Iso2' to 'iso2c'
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso3")] <- "iso3c" # Rename 'Iso3' to 'iso3c'
DataUNPopulation$Year <- as.numeric(DataUNPopulation$Year) # Convert 'Year' to numeric
DataUNPopulation <- merge(subset(DataUNPopulation, select = -Country), subset(CountryAssumptions, select = c(Country, iso3c)), by = "iso3c") # Add iso classification country names to 'DataUNPopulation' based on 'CountryAssumptions'

# Read GDP and Population data from the SSP scenarios, and estimate annual pathways
DataSSPFutureGDP_data <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926)) # Read GDP and Population data from the SSP scenarios
DataSSPFutureGDP <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5)), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5))) # Read metadata only from the SSP scenarios

# Extract data for every 10 years from 2010 to 2100 from the SSP scenarios and estimate annual values inbetween by means of linear interpolation.
for (i in 2010:2100) { 
  if (i %in% seq(2010,2100,10)) {
    DataSSPFutureGDP <- cbind(DataSSPFutureGDP, data = DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",i,sep = ""))])
    colnames(DataSSPFutureGDP)[which(names(DataSSPFutureGDP) == paste("X",i,sep = ""))] <- i
    DataYear = i
  } else {
    DataSSPFutureGDP <- cbind(DataSSPFutureGDP, data = (DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",DataYear+10,sep = ""))]-DataSSPFutureGDP_data[which(names(DataSSPFutureGDP_data) == paste("X",DataYear,sep = ""))])/10 + DataSSPFutureGDP[which(names(DataSSPFutureGDP) == i-1)])
    colnames(DataSSPFutureGDP)[length(names(DataSSPFutureGDP))] <- i
  }
}

DataSSPFutureGDP <- melt(DataSSPFutureGDP, measure.vars = as.character(2010:2100), value.name = "Value", variable.name = "Year") # Reshape the data
DataSSPFutureGDP$Year <- as.character(DataSSPFutureGDP$Year) # Convert 'Year' to character
DataSSPFutureGDP$Year <- as.numeric(DataSSPFutureGDP$Year) # Convert 'Year' to numeric
DataSSPFutureGDP$Region[DataSSPFutureGDP$Region == "World"] <- "WLD" # Replace 'World' with 'WLD' in 'Region'
DataSSPFutureGDP <- rbind(DataSSPFutureGDP, cbind(Model = "Aggregate", Region = "EUU",
  aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% DataWorldBank$iso3c[DataWorldBank$iso2c %in% EUStatesISO2]), sum))) # Aggregate the data for the EU states

# Preparation for analysis
DataGlobalCarbonBudget <- DataGlobalCarbonBudget[order(DataGlobalCarbonBudget$Year),]
DataWorldBank <- DataWorldBank[order(DataWorldBank$Year),]

DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "European Union" & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "European Union" & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Sweden" & DataGlobalCarbonBudget$Accounting == "Territorial Emissions"] #Adjust EU data to exclude Sweden
DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "European Union" & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "European Union" & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Sweden" & DataGlobalCarbonBudget$Accounting == "Consumption Emissions"] #Adjust EU data to exclude Sweden
DataWorldBank$GDP[DataWorldBank$Country == "European Union"] <- DataWorldBank$GDP[DataWorldBank$Country == "European Union"] - DataWorldBank$GDP[DataWorldBank$Country == "Sweden"] #Adjust EU data to exclude Sweden
DataWorldBank$GDPpc[DataWorldBank$Country == "European Union"] <- DataWorldBank$GDP[DataWorldBank$Country == "European Union"] / DataUNPopulation$Population[DataUNPopulation$Country == "European Union" & DataUNPopulation$Year <= 2022]

CountryAssumptions$iso3c[CountryAssumptions$Country == "Rest of world"] <- "ROW" # Replace 'Rest of world' with 'ROW' in 'iso3c'
DataUNPopulation <- rbind(subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c),
                          cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", aggregate(Population ~ Year, subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum))) # Aggregate the population data for the analyzed countries and temporarily store as 'Rest of World'
DataGlobalCarbonBudget <- rbind(subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country & Accounting == "Territorial Emissions" & Year >= 1960, select = -Accounting),
                                cbind(Country = "Rest of world", iso3c = "ROW", aggregate(EmissionsMtCO2 ~ Year, subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country & Accounting == "Territorial Emissions" & Year >= 1960 & Country != "World", select = -Accounting), sum))) # Aggregate the emissions data for the analyzed countries and temporarily store as 'Rest of World'
DataWorldBank <- rbind(subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c),
                       cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", merge(aggregate(GDP ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum), aggregate(GDPpc ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum), by = "Year"))) # Aggregate the current and past GDP for the analyzed countries and temporarily store as 'Rest of World'
DataSSPFutureGDP <- rbind(subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c),
                          cbind(Model = "Aggregate", Region = "ROW", aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c & Region != "WLD"), sum))) # Aggregate future GDP and population for the analyzed countries in the SSP scenarios and temporarily store as 'Rest of World'

DataUNPopulation <- DataUNPopulation[order(DataUNPopulation$Country, DataUNPopulation$Year),] # Order the population data
DataGlobalCarbonBudget <-DataGlobalCarbonBudget[order(DataGlobalCarbonBudget$Country, DataGlobalCarbonBudget$Year),] # Order the Carbon Budget data
DataWorldBank <- DataWorldBank[order(DataWorldBank$Country, DataWorldBank$Year),] # Order the World Bank data
DataSSPFutureGDP <- DataSSPFutureGDP[order(DataSSPFutureGDP$Region, DataSSPFutureGDP$Year),] # Order the future GDP data

DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"] <- DataUNPopulation$Population[DataUNPopulation$Country == "World"] - DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"] # Calculate the population for the rest of the world
DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world"] # Calculate the emissions for the rest of the world
DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "World"] - DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] # Calculate the GDP for the rest of the world
DataWorldBank$GDPpc[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] / DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world" & DataUNPopulation$Year <= 2022] # Calculate the GDP per capita for the rest of the world
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "GDP|PPP"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"] # Calculate the GDP for the rest of the world
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "Population"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"] # Calculate the population for the rest of the world

# Define Annual Capability function based on Supplementary Equation 4 #
AnnualCapability <- function(Country, TempTarget) {
  AnnualBudget <- c()
  for (Year in 2023:2070) {
    AnnualBudget = c(AnnualBudget,
                     GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] * 
                       if (Country == "World") {1} else {
                         (DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
                            DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"]) /
                           sum(DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region != "WLD" & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
                                 DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region != "WLD" & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"])
                       })
  }
  TotalBudget = sum(AnnualBudget)
  return(TotalBudget)
}

# Define Annual Per Capita Convergence function based on Supplementary Equation 3 #
AnnualPerCapitaConvergence <- function(Country, TempTarget, ConvYear) {
  AnnualBudget <- c()
  for (Year in 2023:2070) {
    AnnualBudget = c(AnnualBudget, 
                     GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] *
                       (min((Year-2022)/(ConvYear-2022),1) * 
                          DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year == Year]/
                          DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year == Year] +
                          max(1-(Year-2022)/(ConvYear-2022),0) * 
                          DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022] /
                          DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022]))
  }
  TotalBudget = sum(AnnualBudget)
  return(TotalBudget)
}

#### CALCULATIONS ####

# Calculate the planned emissions for each country from 2023 to 2070,
# based on Equation 3, assumptions in CountryAssumptions.xlsx, and 2022 emissions per country. 
PlannedEmissions <-
  foreach(Country = CountryAssumptions$Country[CountryAssumptions$Country != "World"], .combine = "rbind") %:%
  foreach(Year = 2023:2070, .combine = "rbind") %do% {
    data.frame(Country = Country,
               Year = Year,
               iso3c = CountryAssumptions$iso3c[CountryAssumptions$Country == Country],
               EmissionsMtCO2 = if (Year < 2031) {
                 approx(y = c(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022], 
                              DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == CountryAssumptions$BaseYear2030[CountryAssumptions$Country == Country]] * 
                                (1 + CountryAssumptions$ChangeIn2030[CountryAssumptions$Country == Country])), 
                        x = c(2022,2030), xout = Year, method = "linear")$y
               } else if (Year > 2030 & Year <= CountryAssumptions$NetZeroYear[CountryAssumptions$Country == Country]) {
                 approx(y = c(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == CountryAssumptions$BaseYear2030[CountryAssumptions$Country == Country]] * 
                                (1 + CountryAssumptions$ChangeIn2030[CountryAssumptions$Country == Country]), 0), 
                        x = c(2030,CountryAssumptions$NetZeroYear[CountryAssumptions$Country == Country]), xout = Year, method = "linear")$y
               } else if (Year > CountryAssumptions$NetZeroYear[CountryAssumptions$Country == Country]) {0})
  }


# Calculate the global emission curves for 1.5 and 2 degrees Celsius
GlobalEmissionCurves <- data.frame()
for (TempTarget in c(1.5, 2)) {
  GlobalNetZero = (CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget]*1000+DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022])*2/DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022]+2021
  for (Year in 2023:2070) {
    GlobalEmissionCurves <- rbind(GlobalEmissionCurves, data.frame(TempTarget = TempTarget, Year = Year,
                                                                   EmissionsMtCO2 = if (Year <= GlobalNetZero) {approx(y = c(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022],0),
                                                                                                                       x = c(2022,GlobalNetZero), xout = Year, method = "linear")$y} else {0}))
  }
  GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == floor(GlobalNetZero)] <- GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == floor(GlobalNetZero)] + CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget]*1000 - sum(GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget])
}


Cluster <- makeCluster(NoCores) # Set up a cluster for parallel processing
registerDoParallel(Cluster) # Register the cluster for parallel processing

# Calculate the additional carbon accountability for each country based on different allocation principles
AdditionalCarbonAccountability <-
  foreach(TempTarget = c(1.5, 2), .combine = "rbind") %:%
  foreach(AllocationPrincipleCB = c("Equal cumulative per capita (main case)", "Equal cumulative per capita (whole period)", "Grandfathering", "Capability", "Contraction and convergence"), .combine = "rbind") %:%
  foreach(AllocationPrincipleEAP = c("Equal cumulative per capita (main case)", "Equal cumulative per capita (whole period)", "Grandfathering", "Capability", "Contraction and convergence"), .combine = "rbind") %:%
  foreach(CarbonDebtAssumption = c(1990, 1960, 1980, 2000, FALSE), .combine = "rbind") %dopar% {
    AdditionalCarbonAccountabilityCalculation = data.frame()
    for (Country in CountryAssumptions$Country) {
      ## STEP 1 ##
      CarbonDebt = if (CarbonDebtAssumption != FALSE & AllocationPrincipleCB != "Equal cumulative per capita (whole period)") { #Equation 1
        sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= CarbonDebtAssumption]) -
          sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= CarbonDebtAssumption]) * 
          sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= CarbonDebtAssumption & DataUNPopulation$Year <= 2022]) /
          sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= CarbonDebtAssumption & DataUNPopulation$Year <= 2022])
      } else {0}
      ## STEP 2 ##
      NationalFutureCarbonBudget = 
        if (AllocationPrincipleCB == "Equal cumulative per capita (main case)") { #Estimating the second term of Equation 2
          CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 * 
            sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year > 2022]) /
            sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year > 2022])
        } else if (AllocationPrincipleCB == "Grandfathering") { #Estimating the second term of Equation 2 based on Supplementary Equation 1
          CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 * 
            DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022] /
            DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022]
        } else if (AllocationPrincipleCB == "Equal cumulative per capita (whole period)") { #Estimating the second term of Equation 2 based on Supplementary Equation 3
          (CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 + sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)])) *
            sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)]) /
            sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)])
        } else if (AllocationPrincipleCB == "Contraction and convergence") { #Estimating the second term of Equation 2 based on Supplementary Equation 5
          AnnualPerCapitaConvergence(Country, TempTarget, 2040)
        } else if (AllocationPrincipleCB == "Capability") { #Estimating the second term of Equation 2 based on Supplementary Equation 7
          AnnualCapability(Country, TempTarget)
        }
      ExcessiveCarbonClaims = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2), sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])) + ifelse(AllocationPrincipleCB == "Equal cumulative per capita (whole period)",sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)]),0) - NationalFutureCarbonBudget #Equation 2
      TotalExcessiveCarbonClaims = CarbonDebt + ExcessiveCarbonClaims # Equation 4
      AdditionalCarbonAccountabilityCalculation = rbind(AdditionalCarbonAccountabilityCalculation, data.frame(Country = Country, 
                                                                                                              EconomicDevelopment = CountryAssumptions$Development[CountryAssumptions$Country == Country], 
                                                                                                              TempTarget = TempTarget, 
                                                                                                              AllocationPrincipleCB = AllocationPrincipleCB, 
                                                                                                              AllocationPrincipleEAP = AllocationPrincipleEAP, 
                                                                                                              CarbonDebtAssumption = CarbonDebtAssumption, 
                                                                                                              GDPPerCapita = DataWorldBank$GDPpc[DataWorldBank$Year == 2022 & DataWorldBank$Country == Country], 
                                                                                                              CarbonDebt = CarbonDebt, 
                                                                                                              FutureCarbonClaims = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2), sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])), 
                                                                                                              ExcessiveCarbonClaims = ExcessiveCarbonClaims, 
                                                                                                              TotalExcessiveCarbonClaims = TotalExcessiveCarbonClaims))
    }
    ## STEP 3 ##
    EmissionAllowancesPool = -sum(AdditionalCarbonAccountabilityCalculation$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityCalculation$TotalExcessiveCarbonClaims < 0]) # Equation 5
    AdditionalCarbonAccountabilityIteration <- cbind(Iteration = 0, subset(AdditionalCarbonAccountabilityCalculation, select = c(Country, TotalExcessiveCarbonClaims))) # Preparation for iteration
    n = 0 # Iteration number
    while (EmissionAllowancesPool > 0) { # Iteratively solve Equation 7
      n = n + 1 # Increase iteration number
      AdditionalCarbonAccountabilityIteration <- rbind(AdditionalCarbonAccountabilityIteration, cbind(Iteration = n, subset(AdditionalCarbonAccountabilityIteration, Iteration == 0, select = -Iteration)))
      for (Country in CountryAssumptions$Country) {
        if (Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountabilityIteration$Country != "World"]) {
          if (AllocationPrincipleEAP %in% c("Equal cumulative per capita (main case)", "Equal cumulative per capita (whole period)")) { # Allocating the EAP based on Equation 6 and Supplementary Equation 4
            AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = 
              AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1] - 
              EmissionAllowancesPool*
              (sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070])/
                 sum(DataUNPopulation$Population[DataUNPopulation$Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountabilityIteration$Country != "World"] & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]))
          } else if (AllocationPrincipleEAP == "Grandfathering") { # Allocating the EAP based Supplementary Equation 2
            AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = 
              AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1] - 
              EmissionAllowancesPool*
              (sum(AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1])/
                 sum(AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountabilityIteration$Country != "World"] & AdditionalCarbonAccountabilityIteration$Iteration == n-1]))
          } else if (AllocationPrincipleEAP == "Contraction and convergence") { # Allocating the EAP based Supplementary Equation 6
            AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = 
              AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1] - 
              EmissionAllowancesPool*
              AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1] * sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070])/
              sum(AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountabilityIteration$Country != "World"] & AdditionalCarbonAccountabilityIteration$Iteration == n-1] *
                             aggregate(Population ~ Country, subset(DataUNPopulation, Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountabilityIteration$Country != "World"] & Year >= 2023 & Year <= 2070), sum)$Population)
          } else if (AllocationPrincipleEAP == "Capability") { # Allocating the EAP based Supplementary Equation 8
            AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = 
              AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n-1] - 
              EmissionAllowancesPool*
              (sum(DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Year >= 2023 & DataSSPFutureGDP$Year <= 2070 & DataSSPFutureGDP$Variable == "GDP|PPP"])/
                 sum(DataSSPFutureGDP$Value[DataSSPFutureGDP$Region %in% CountryAssumptions$iso3c[CountryAssumptions$Country %in% AdditionalCarbonAccountabilityIteration$Country[AdditionalCarbonAccountabilityIteration$Iteration == n-1 & AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims > 0]] & DataSSPFutureGDP$Region != "WLD" & DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Year >= 2023 & DataSSPFutureGDP$Year <= 2070 & DataSSPFutureGDP$Variable == "GDP|PPP"]))
          }
        } else if (Country == "World") { # Global additional carbon accountability equals the net global excessive carbon claims.
          AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = sum(AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country != "World" & AdditionalCarbonAccountabilityIteration$Iteration == n]) 
        } else { # Assign zero Additional Carbon Accountability to countries with negative total excessive carbon claims
          AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] = 0 
        }
      }
      # Estimate the remaining emission allowances pool, EAP, for the iteration.
      EmissionAllowancesPool <- -sum(AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims < 0 & AdditionalCarbonAccountabilityIteration$Iteration == n]) 
    }
    AdditionalCarbonAccountabilityCalculation = cbind(AdditionalCarbonAccountabilityCalculation, AdditionalCarbonAccountability = 0, AdditionalCarbonAccountabilityPerCapita = 0)
    for (Country in CountryAssumptions$Country) { # Calculate the Additional Carbon Accountability per capita based on average population during the period 2023-2070.
      AdditionalCarbonAccountabilityCalculation$AdditionalCarbonAccountability[AdditionalCarbonAccountabilityCalculation$Country == Country] <- AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n]
      AdditionalCarbonAccountabilityCalculation$AdditionalCarbonAccountabilityPerCapita[AdditionalCarbonAccountabilityCalculation$Country == Country] <- AdditionalCarbonAccountabilityIteration$TotalExcessiveCarbonClaims[AdditionalCarbonAccountabilityIteration$Country == Country & AdditionalCarbonAccountabilityIteration$Iteration == n] / 
        mean(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070])*1e6
    }
    data.frame(AdditionalCarbonAccountabilityCalculation)
  }
stopCluster(Cluster) # Stop the cluster
rm(Cluster) # Remove the cluster

#Calculate carbon debt for each country for the period 1990-2021 based on both territorial and consumption-based accounting frameworks. Note that this calculation excludes Norway and Gambia and, also, emissions for the year 2022 for all countries, due to lack of data.
CarbonDebtConsumptionSensitivity <-
  foreach(Country = CountryAssumptions$Country[!CountryAssumptions$Country %in% c("Norway", "Gambia", "World", "Rest of world")], .combine = "rbind") %:%
  foreach(Accounting = c("Territorial Emissions", "Consumption Emissions"), .combine = "rbind") %do% {
    data.frame(
      Country = Country,
      Accounting = Accounting,
      CarbonDebt = sum(DataGlobalCarbonBudgetALL$EmissionsMtCO2[DataGlobalCarbonBudgetALL$Country == Country & DataGlobalCarbonBudgetALL$Year >= 1990 & DataGlobalCarbonBudgetALL$Year <= 2021 & DataGlobalCarbonBudgetALL$Accounting == Accounting]) -
        sum(DataGlobalCarbonBudgetALL$EmissionsMtCO2[DataGlobalCarbonBudgetALL$Country == "World" & DataGlobalCarbonBudgetALL$Year >= 1990 & DataGlobalCarbonBudgetALL$Year <= 2021 & DataGlobalCarbonBudgetALL$Accounting == Accounting]) * 
        sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2021]) /
        sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2021])
    )
  }
CarbonDebtConsumptionSensitivity <- cbind(CarbonDebtConsumptionSensitivity, ChangeTerrToCons = 0) # Add column for change in percent if switching from territorial to consumption-based accounting
CarbonDebtConsumptionSensitivity$ChangeTerrToCons[CarbonDebtConsumptionSensitivity$Accounting == "Consumption Emissions"] <- CarbonDebtConsumptionSensitivity$CarbonDebt[CarbonDebtConsumptionSensitivity$Accounting == "Consumption Emissions"]/CarbonDebtConsumptionSensitivity$CarbonDebt[CarbonDebtConsumptionSensitivity$Accounting == "Territorial Emissions"]-1

#### PREPARATION OF RESULTS FOR PLOTS AND EXPORT TO EXCEL ####
DataAnnualEmissionsFigure <- cbind(PastFuture = "", merge(subset(rbind(DataGlobalCarbonBudget, PlannedEmissions), !Country %in% c("World", "Rest of world") & Year >= 2000), subset(DataUNPopulation, !Country %in% c("World", "Rest of world") & Year >= 2000), by = c("Country", "Year"))) # Merge the data for the emissions figure
DataAnnualEmissionsFigure <- merge(DataAnnualEmissionsFigure, subset(CountryAssumptions, select = c(Country, Development)), by = "Country") # Merge the data for the emissions figure
DataAnnualEmissionsFigure$PastFuture[DataAnnualEmissionsFigure$Year > 2022] = "Planned emissions" # Define the past and future emissions > 2022
DataAnnualEmissionsFigure$PastFuture[DataAnnualEmissionsFigure$Year <= 2022] = "Historic emissions" # Define the past and future emissions <= 2022 
DataAnnualEmissionsFigure <- rbind(DataAnnualEmissionsFigure, cbind(PastFuture = "Planned emissions", subset(DataAnnualEmissionsFigure, Year == 2022, select = -PastFuture))) # Add the planned emissions for 2022
DataAnnualEmissionsFigure$Development <- factor(DataAnnualEmissionsFigure$Development, levels = c("High", "Upper-middle", "Lower-middle", "Low"), labels = c("High\nincome", "Upper-middle\nincome", "Lower-middle\nincome", "Low\nincome")) # Define the development levels
CountriesPlannedEmissionsFig <- c("United States", "European Union", "Brazil", "Russia", "India", "China", "South Africa", "Iran", "United Arab Emirates", "Ethiopia") # Define the countries for the planned emissions figure for selected countries

# Create a table with results for the manuscript and the data for the Excel-calculation
TableForManuscript <- subset(AdditionalCarbonAccountability, AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & !Country %in% c("World" ,"Rest of world")) # Subset the data for the table
TableForManuscript <- TableForManuscript[order(TableForManuscript$TempTarget, -TableForManuscript$GDPPerCapita),] # Order the data for the table
TableForManuscript <- rbind(TableForManuscript, subset(AdditionalCarbonAccountability, AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Country %in% c("World" ,"Rest of world"))) # Add the data for the rest of the world

TableDataForExcel <- data.frame()
for (Country in CountryAssumptions$Country) {
  TableDataForExcel <- rbind(TableDataForExcel, data.frame(
    Country = Country, # Country
    Population2022 = DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year == 2022], # Population in 2022
    AggregatedPopulation19902022 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]), # Aggregated population from 1990 to 2022
    AggregatedPopulation20232070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]), # Aggregated population from 2023 to 2070
    AggregatedPopulation19902070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]), # Aggregated population from 1990 to 2070
    ShareinWorldPopulation2022 = DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year == 2022] / DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year == 2022], # Share in the world population in 2022
    ShareinWorldPopulation19902022 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]), # Share in the world population from 1990 to 2022
    ShareinWorldPopulation20232070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]), # Share in the world population from 2023 to 2070
    ShareinWorldPopulation19902070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]), # Share in the world population from 1990 to 2070
    NetZeroYear = CountryAssumptions$NetZeroYear[CountryAssumptions$Country == Country], # Net zero year
    HistoricEmissions2022MtCO2 = DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022], # Emissions in 2022
    HistoricEmissions19902022MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]), # Historic emissions from 1990 to 2022
    PlannedEmissions20232070MtCO2 = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])), # Planned emissions from 2023 to 2070
    TotalEmissions19902070MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) + ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])), # Total emissions from 1990 to 2070
    ShareinWorldEmissions2022MtCO2 = DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022] / DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022], # Share in world emissions in 2022
    ShareinWorldEmissions19902022MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) / sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= 1990]), # Share in world emissions from 1990 to 2022
    ShareinWorldEmissions20232070MtCO2 = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])) / sum(PlannedEmissions$EmissionsMtCO2), # Share in world emissions from 2023 to 2070
    ShareinWorldEmissions19902070MtCO2 = (sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) + ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country]))) / (sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= 1990]) + sum(PlannedEmissions$EmissionsMtCO2)), # Share in world emissions from 1990 to 2070
    GDP2021 = DataWorldBank$GDP[DataWorldBank$Country == Country & DataWorldBank$Year == 2021], # GDP in 2021
    GDPpercapita2021 = DataWorldBank$GDPpc[DataWorldBank$Country == Country & DataWorldBank$Year == 2021] # GDP per capita in 2021
  ))
}

# Loads the Excel-file with pre-defined Excel-calculations and replaces the data to match the R-calculations
if (file.exists("TablesInManuscriptandCalculationsSwedenExplicit.xlsx") == TRUE) {
  XlsxRepository <- loadWorkbook("TablesInManuscriptandCalculationsSwedenExplicit.xlsx") # Load the Excel file
  removeSheet(XlsxRepository, sheetName = "Table 2") # Remove the sheet for the results for the 1.5 degrees Celsius target
  removeSheet(XlsxRepository, sheetName = "SupplementaryTable1") # Remove the sheet for the results for the 2 degrees Celsius target
  removeSheet(XlsxRepository, sheetName = "DataFromRScriptForCalculations") # Remove the sheet for the data
  saveWorkbook(XlsxRepository, "TablesInManuscriptandCalculationsSwedenExplicit.xlsx") # Save the Excel file
}

write.xlsx(subset(TableForManuscript, TempTarget == 1.5), "TablesInManuscriptandCalculationsSwedenExplicit.xlsx", sheetName = "Table 2", row.names = FALSE, append = TRUE) # Write the table for the 1.5 degrees Celsius target
write.xlsx(subset(TableForManuscript, TempTarget == 2), "TablesInManuscriptandCalculationsSwedenExplicit.xlsx", sheetName = "SupplementaryTable1", row.names = FALSE, append = TRUE) # Write the table for the 2 degrees Celsius target
write.xlsx(TableDataForExcel, "TablesInManuscriptandCalculationsSwedenExplicit.xlsx", sheetName = "DataFromRScriptForCalculations", row.names = FALSE, append = TRUE) # Write the data for the table

# Write Excel-file with estimated planned emissions for all countries
TablewithPlannedEmissions <- dcast(PlannedEmissions, Country ~ Year, value.var = "EmissionsMtCO2") # Create a table with the planned emissions
write.xlsx(TablewithPlannedEmissions, "PlannedEmissionsSwedenExplicit.xlsx", sheetName = "EmissionsMtCO2", row.names = FALSE) # Write the table with the planned emissions

# Prepare data for figures on the Excessive Carbon Claims vs. Carbon Debt
DataForZoomFigure <- subset(AdditionalCarbonAccountability, AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & Country != "World" & CarbonDebtAssumption == 1990) # Subset the data for the zoom figure
DataForZoomFigureLabels <- cbind(DataForZoomFigure, zoom = TRUE) # Add a column for the zoom figure
ZoomLevel <- 22000 # Define the zoom level
DataForZoomFigureLabels$zoom <- ifelse(DataForZoomFigureLabels$CarbonDebt > -ZoomLevel & DataForZoomFigureLabels$CarbonDebt < ZoomLevel & DataForZoomFigureLabels$ExcessiveCarbonClaims > -ZoomLevel & DataForZoomFigureLabels$ExcessiveCarbonClaims < ZoomLevel, TRUE, FALSE) # Define the relative zoom level of the figure

DataForZoomFigurePerCapita <- cbind(DataForZoomFigure, CarbonDebtPerCapita = DataForZoomFigure$CarbonDebt, ExcessiveCarbonClaimsPerCapita = DataForZoomFigure$ExcessiveCarbonClaims) # Add columns for the per capita values
# go through for each country 
for (Country in CountryAssumptions$Country) {
  DataForZoomFigurePerCapita$CarbonDebtPerCapita[DataForZoomFigurePerCapita$Country == Country] <- DataForZoomFigurePerCapita$CarbonDebtPerCapita[DataForZoomFigurePerCapita$Country == Country] / mean(DataUNPopulation$Population[DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022 & DataUNPopulation$Country == Country]) * 1e6 # Calculate the per capita carbon debt
  DataForZoomFigurePerCapita$ExcessiveCarbonClaimsPerCapita[DataForZoomFigurePerCapita$Country == Country] <- DataForZoomFigurePerCapita$ExcessiveCarbonClaimsPerCapita[DataForZoomFigurePerCapita$Country == Country] / mean(DataUNPopulation$Population[DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070 & DataUNPopulation$Country == Country]) * 1e6 # Calculate the per capita excessive carbon claims
}
DataForZoomFigureLabelsPerCapita <- cbind(DataForZoomFigurePerCapita, zoom = TRUE) # Add a column for the zoom figure
ZoomLevelPerCapita <- 150 # Define the zoom level for the per capita values
DataForZoomFigureLabelsPerCapita$zoom <- ifelse(DataForZoomFigureLabelsPerCapita$CarbonDebtPerCapita > -ZoomLevelPerCapita & DataForZoomFigureLabelsPerCapita$CarbonDebtPerCapita < ZoomLevelPerCapita & DataForZoomFigureLabelsPerCapita$ExcessiveCarbonClaimsPerCapita > -ZoomLevelPerCapita & DataForZoomFigureLabelsPerCapita$ExcessiveCarbonClaimsPerCapita < ZoomLevelPerCapita, TRUE, FALSE) # Define the relative zoom level of the figure

DataForZoomFigure$EconomicDevelopment <- factor(DataForZoomFigure$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world")) # Define the economic development levels
DataForZoomFigureLabels$EconomicDevelopment <- factor(DataForZoomFigureLabels$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world")) # Define the economic development levels
DataForZoomFigurePerCapita$EconomicDevelopment <- factor(DataForZoomFigurePerCapita$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world")) # Define the economic development levels
DataForZoomFigureLabelsPerCapita$EconomicDevelopment <- factor(DataForZoomFigureLabelsPerCapita$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world")) # Define the economic development levels

# Prepare data for figures on the sensitivity analysis and export data to an Excel-file.
DataFiguresSensitivity <- cbind(AdditionalCarbonAccountability, ChangeComparedToMain = 0, ChangeComparedToMainPerCapita = 0, TotExcessiveChangeToMain = 0) # Add empty columns for the sensitivity analysis
DataFiguresSensitivity <- DataFiguresSensitivity[order(DataFiguresSensitivity$TempTarget, DataFiguresSensitivity$AllocationPrincipleCB, DataFiguresSensitivity$AllocationPrincipleEAP, DataFiguresSensitivity$CarbonDebtAssumption, DataFiguresSensitivity$Country),] # Order the data
for (iCB in unique(DataFiguresSensitivity$AllocationPrincipleCB)) {
  for (iEAP in unique(DataFiguresSensitivity$AllocationPrincipleEAP)) {
    for (j in unique(DataFiguresSensitivity$CarbonDebtAssumption)) {
      DataFiguresSensitivity$ChangeComparedToMain[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$AdditionalCarbonAccountability[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$AdditionalCarbonAccountability[DataFiguresSensitivity$AllocationPrincipleCB == "Equal cumulative per capita (main case)" & DataFiguresSensitivity$AllocationPrincipleEAP == "Equal cumulative per capita (main case)"  & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
      DataFiguresSensitivity$ChangeComparedToMainPerCapita[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$AdditionalCarbonAccountabilityPerCapita[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$AdditionalCarbonAccountabilityPerCapita[DataFiguresSensitivity$AllocationPrincipleCB == "Equal cumulative per capita (main case)" & DataFiguresSensitivity$AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
      DataFiguresSensitivity$TotExcessiveChangeToMain[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$TotalExcessiveCarbonClaims[DataFiguresSensitivity$AllocationPrincipleCB == iCB & DataFiguresSensitivity$AllocationPrincipleEAP == iEAP & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$TotalExcessiveCarbonClaims[DataFiguresSensitivity$AllocationPrincipleCB == "Equal cumulative per capita (main case)" & DataFiguresSensitivity$AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
    }
  }
}
DataFiguresSensitivity <- DataFiguresSensitivity[order(DataFiguresSensitivity$TempTarget, DataFiguresSensitivity$AllocationPrincipleCB, DataFiguresSensitivity$AllocationPrincipleEAP, DataFiguresSensitivity$CarbonDebtAssumption, DataFiguresSensitivity$EconomicDevelopment), ] # Order the data
CountriesForSensitivity = subset(DataWorldBank, Year == 2022, select = c(Country,GDPpc)) # Get data on GDP for 2022 for countries to order the results
CountriesForSensitivity <- CountriesForSensitivity[order(CountriesForSensitivity$GDPpc), ] # Order the countries according to GDP for 2022
DataFiguresSensitivity$Country <- factor(DataFiguresSensitivity$Country, levels = CountriesForSensitivity$Country) # Order the countries in the sensitivity analysis 
DataFiguresSensitivity$AdditionalCarbonAccountability <- DataFiguresSensitivity$AdditionalCarbonAccountability/1e3 # Convert the additional carbon accountability in the sensitivity analysis to GtCO2
CarbonDebtConsumptionSensitivity$Country <- factor(CarbonDebtConsumptionSensitivity$Country, levels = CountriesForSensitivity$Country) # Order the countries in the comparison of using consumption-based emission allocation for the carbon debt

write.xlsx(DataFiguresSensitivity, "DataForSensitivityAnalysisSwedenExplicit.xlsx", row.names = FALSE) # Write the data for the sensitivity analysis

DataFiguresSensitivity$CarbonDebtAssumption[DataFiguresSensitivity$CarbonDebtAssumption == FALSE] <- 9999
DataFiguresSensitivity <- melt(DataFiguresSensitivity, measure.vars = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), variable.name = "Graph", value.name = "Change") # Merge the data for the sensitivity analysis
DataFiguresSensitivity$Graph <- factor(DataFiguresSensitivity$Graph, levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                       labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))) # Define the labels for the graphs



#### PLOTTING FIGURES FOR MAIN ARTICLE ####
# Plot Figure 1 on planned emissions for selected countries.
FigurePlannedEmissions <- ggplot() +
  geom_line(data = cbind(subset(DataAnnualEmissionsFigure, Country %in% CountriesPlannedEmissionsFig), Graph = factor("Total", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")"))))), mapping = aes(x = Year, y = EmissionsMtCO2/1e3, color = Development, linetype = PastFuture, alpha = PastFuture, size = Country)) +
  geom_line(data = cbind(subset(DataAnnualEmissionsFigure, Country %in% CountriesPlannedEmissionsFig), Graph = factor("PerCapita", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")"))))), mapping = aes(x = Year, y = EmissionsMtCO2/Population*1e6, color = Development, linetype = PastFuture, alpha = PastFuture, size = Country)) +
  geom_text_repel(data = cbind(subset(DataAnnualEmissionsFigure, Country %in% CountriesPlannedEmissionsFig & Year == 2022 & PastFuture == "Historic emissions"), Graph = factor("Total", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")"))))), 
                  size = 1.7, show.legend = FALSE,
                  hjust = 0,
                  box.padding = .2,
                  segment.angle = 20,
                  segment.curvature = -0.1,
                  segment.size = .3,
                  segment.alpha = .5,
                  min.segment.length = 0.05, 
                  xlim = c(NA, 2022),
                  mapping = aes(label = Country, x = Year, y = EmissionsMtCO2/1e3, color = Development)) +
  geom_text_repel(data = cbind(subset(DataAnnualEmissionsFigure, Country %in% CountriesPlannedEmissionsFig & Year == 2022 & PastFuture == "Historic emissions"), Graph = factor("PerCapita", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")"))))), 
                  size = 1.7, show.legend = FALSE,
                  hjust = 0,
                  box.padding = .2,
                  segment.angle = 20,
                  segment.curvature = -0.1,
                  segment.size = .3,
                  segment.alpha = .5,
                  min.segment.length = 0.05, 
                  xlim = c(NA, 2022),
                  mapping = aes(label = Country, x = Year, y = EmissionsMtCO2/Population*1e6, color = Development)) +
  geom_text(size = 2.5, data = rbind(data.frame(Text = "a)", Graph = factor("Total", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")"))))),
                                     data.frame(Text = "b)", Graph = factor("PerCapita", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")")))))), 
            mapping = aes(x = 2000, y = ifelse(Graph == factor("Total", levels = c("Total", "PerCapita"), labels = c(expression(paste("Annual emissions (Gt", CO[2],")")), expression(paste("Annual emissions per capita (t", CO[2],")")))), 11, 33), label = Text)) +
  facet_wrap(~Graph, scales = "free", nrow = 2, strip.position = "left",
             labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2000, 2070, 10)) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_manual(values = scico(4, palette = "roma")) +
  scale_alpha_manual(values = c(0.25,1)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = rep(0.5, 10)) +
  guides(size = "none") +
  coord_cartesian(clip = "off", xlim = c(2000,2070)) +
  labs(x = NULL, y = NULL, color = NULL, linetype = NULL, alpha = NULL) +
  theme_bw(base_size = 8) + theme(strip.background = element_blank(),
                                  strip.text = element_text(color = "black"),
                                  strip.placement = "outside",
                                  legend.position = "bottom",
                                  legend.box = "vertical",
                                  legend.margin = margin(0),
                                  legend.box.margin = margin(0),
                                  legend.spacing = unit(0,"pt"))

png(filename = "Figure 1 - FigurePlannedEmissions.png", width = 88, height = 120,  units = "mm", res = 500) # Save the figure 1. as a PNG file
print(FigurePlannedEmissions) # Print the figure
dev.off() # Close the PNG file

pdf(file = "Figure 1 - FigurePlannedEmissions.pdf", width = 88/25.4, height = 120/25.4) # Save the figure 1. as a PDF file
print(FigurePlannedEmissions) # Print the figure
dev.off() # Close the PDF file

### Plot Figure 2 for the per capita values
CarbonDebtvsExcessiveClaimsPerCapita <- ggplot(mapping = aes(x = CarbonDebtPerCapita, y = ExcessiveCarbonClaimsPerCapita, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_hline(data = subset(DataForZoomFigure, TempTarget == 1.5), mapping = aes(yintercept = 0), color = "gray", size = .5) +
  geom_vline(data = subset(DataForZoomFigure, TempTarget == 1.5), mapping = aes(xintercept = 0), color = "gray", size = .5) +
  geom_point(data = subset(DataForZoomFigurePerCapita, TempTarget == 1.5), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabelsPerCapita, TempTarget == 1.5), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-150,-200), y = c(-150, -150), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevelPerCapita, ZoomLevelPerCapita), ylim = c(-ZoomLevelPerCapita,ZoomLevelPerCapita), zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(4, palette = "roma"), "gray")) +
  labs(x = expression(paste("Carbon debt per capita (t", CO[2],")")), y = expression(paste("Excessive carbon claims per capita (t", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = "inside",
                                     legend.position.inside = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Figure 2 - CarbonDebtvsExcessiveClaimsPerCapita.png", width = 88*2, height = 88,  units = "mm", res = 500) # Save the figure as a PNG file
print(CarbonDebtvsExcessiveClaimsPerCapita) # Print the figure
dev.off() # Close the PNG file

pdf(file = "Figure 2 - CarbonDebtvsExcessiveClaimsPerCapita.pdf", width = 88*2/25.4, height = 88/25.4) # Save the figure as a PDF file
print(CarbonDebtvsExcessiveClaimsPerCapita) # Print the figure
dev.off() # Close the PDF file  

#### PLOTTING FIGURES FOR SUPPLEMENTARY INFORMATION ####
# Plot Supplementary Figure 1 for the excessive carbon claims vs. carbon debt
CarbonDebtvsExcessiveClaims <- ggplot(mapping = aes(x = CarbonDebt/1000, y = ExcessiveCarbonClaims/1000, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_hline(data = subset(DataForZoomFigure, TempTarget == 1.5), mapping = aes(yintercept = 0), color = "gray", size = .5) +
  geom_vline(data = subset(DataForZoomFigure, TempTarget == 1.5), mapping = aes(xintercept = 0), color = "gray", size = .5) +
  geom_point(data = subset(DataForZoomFigure, TempTarget == 1.5), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, TempTarget == 1.5), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-22,-120), y = c(-22, -20), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevel, ZoomLevel)/1000, ylim = c(-ZoomLevel,ZoomLevel)/1000, zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(4, palette = "roma"), "gray")) +
  labs(x = expression(paste("Carbon Debt (Gt", CO[2],")")), y = expression(paste("Excessive Carbon Claims (Gt", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = "inside",
                                     legend.position.inside = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Supplementary Figure 1 - CarbonDebtvsExcessiveClaims.png", width = 88*2, height = 88,  units = "mm", res = 500) # Save the figure as a PNG file
print(CarbonDebtvsExcessiveClaims) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 2 for the per capita values
CarbonDebtvsExcessiveClaims2CPerCapita <- ggplot(mapping = aes(x = CarbonDebtPerCapita, y = ExcessiveCarbonClaimsPerCapita, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_hline(data = subset(DataForZoomFigure, TempTarget == 2), mapping = aes(yintercept = 0), color = "gray", size = .5) +
  geom_vline(data = subset(DataForZoomFigure, TempTarget == 2), mapping = aes(xintercept = 0), color = "gray", size = .5) +
  geom_point(data = subset(DataForZoomFigurePerCapita, TempTarget == 2), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabelsPerCapita, TempTarget == 2), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-150,-150), y = c(-150, -150), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevelPerCapita, ZoomLevelPerCapita), ylim = c(-ZoomLevelPerCapita,ZoomLevelPerCapita), zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(4, palette = "roma"), "gray")) +
  labs(x = expression(paste("Carbon debt per capita (t", CO[2],")")), y = expression(paste("Excessive carbon claims per capita (t", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = "inside",
                                     legend.position.inside = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Supplementary Figure 2 - CarbonDebtvsExcessiveClaims 2C Per Capita.png", width = 88*2, height = 88,  units = "mm", res = 500) # Save the figure as a PNG file
print(CarbonDebtvsExcessiveClaims2CPerCapita) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 3 for the excessive carbon claims vs. carbon debt for the 2 degrees Celsius target
CarbonDebtvsExcessiveClaims2C <- ggplot(mapping = aes(x = CarbonDebt/1000, y = ExcessiveCarbonClaims/1000, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_hline(data = subset(DataForZoomFigure, TempTarget == 2), mapping = aes(yintercept = 0), color = "gray", size = .5) +
  geom_vline(data = subset(DataForZoomFigure, TempTarget == 2), mapping = aes(xintercept = 0), color = "gray", size = .5) +
  geom_point(data = subset(DataForZoomFigure, TempTarget == 2), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, TempTarget == 2), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-22,-120), y = c(-22, -120), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevel, ZoomLevel)/1000, ylim = c(-ZoomLevel,ZoomLevel)/1000, zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(4, palette = "roma"), "gray")) +
  labs(x = expression(paste("Carbon Debt (Gt", CO[2],")")), y = expression(paste("Excessive Carbon Claims (Gt", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = "inside",
                                     legend.position.inside = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Supplementary Figure 3 - CarbonDebtvsExcessiveClaims 2C.png", width = 88*2, height = 88,  units = "mm", res = 500) # Save the figure as a PNG file
print(CarbonDebtvsExcessiveClaims2C) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 4 on the sensitivity to the start year for the carbon debt
SensitivityAnalysisCarbonDebtAssumption <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & (AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)") & Graph != factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                       labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = as.character(CarbonDebtAssumption)), position = "dodge") +
  geom_rect(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & (AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)") & CarbonDebtAssumption != 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                       labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white") +
  geom_text(size = 3, data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & (AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)") & CarbonDebtAssumption != 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                        labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = ifelse(CarbonDebtAssumption == 1960, 0.5, ifelse(CarbonDebtAssumption == 1980, 1.5, ifelse(CarbonDebtAssumption == 2000, 2.5,3.5))), y = Country, label = ifelse(Change == Inf, "+", ifelse(Change == -100, "-", paste0(round(Change, 0),"%"))), color = as.character(CarbonDebtAssumption))) +
  geom_hline(yintercept = seq(0.5, 38,1), linewidth = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 225, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 800, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 4, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  geom_vline(data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
             mapping = aes(xintercept = Change), linetype = "dashed") +
  geom_text(size = 2.5, data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                          labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = Change, y = "India", label = "World average", angle = -90, vjust = -.5)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 6, limits = c(0,NA)) +
  facetted_pos_scales(x = list(Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))) ~ scale_x_continuous(breaks = -1, limits = c(0,NA)))) +
  scale_fill_manual(values = c(scico(4, palette = "roma")[2:3], scico(4, palette = "roma")[1], scico(4, palette = "roma")[4:5]),
                    labels = c("1960", "1980", "Main case: 1990", "2000", "No Carbon Debt")) +
  scale_color_manual(values = c(scico(4, palette = "roma")[2:3], scico(4, palette = "roma")[4:5]),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                                  strip.background = element_blank(),
                                  strip.placement = "outside",
                                  panel.grid.major.y = element_blank())

png(filename = "Supplementary Figure 4 - SensitivityAnalysisCarbonDebtAssumption.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500) # Save the figure as a PNG file
print(SensitivityAnalysisCarbonDebtAssumption) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 5 on the sensitivity to the assumed allocation principle for only carbon budget (only CB)
SensitivityAnalysisAllocationPrincipleOnlyCB <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph != factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                   labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = AllocationPrincipleCB), position = "dodge") +
  geom_rect(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                    labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white") +
  geom_text(size = 3, data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & AllocationPrincipleCB != "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                    labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = ifelse(AllocationPrincipleCB == "Grandfathering", 0.5, 0) +
                            ifelse(AllocationPrincipleCB == "Equal cumulative per capita (whole period)", 1.5, 0) +
                            ifelse(AllocationPrincipleCB == "Contraction and convergence", 2.5, 0) +
                            ifelse(AllocationPrincipleCB == "Capability", 3.5, 0), y = Country, label = ifelse(Change == Inf, "+", ifelse(Change == -100, "-", paste0(round(Change, 0),"%"))), color = AllocationPrincipleCB)) +
  geom_hline(yintercept = seq(0.5, 38,1), linewidth = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 200, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 800, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 4, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  geom_vline(data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                   labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
             mapping = aes(xintercept = Change), linetype = "dashed") +
  geom_text(size = 2.5, data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                              labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = Change, y = "India", label = "World average", angle = -90, vjust = -.5)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 6, limits = c(0,NA)) +
  facetted_pos_scales(x = list(Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))) ~ scale_x_continuous(breaks = -1, limits = c(0,NA)))) +
  scale_fill_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[1], scico(4, palette = "roma")[3:5]),
                    labels = c("Capability\n(only for the carbon budget\n- based on annual GDP per\ncapita 2023-2070)", 
                               "Main case:\nEqual cumulative emissions per capita", 
                               "Contraction and convergence\n(only for the carbon budget\n- reaching equal annual\nemissions per capita by 2040)", 
                               "Equal cumulative emissions per capita\n(only for the carbon budget\n- cumulative emissions over\nthe period 1990-2070)", 
                               "Grandfathering\n(only for the carbon budget\n- based on share in 2022 global\nemissions)")) +
  scale_color_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[3:5]),
                     guide = "none") + 
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                                  strip.background = element_blank(),
                                  strip.placement = "outside",
                                  panel.grid.major.y = element_blank())

png(filename = "Supplementary Figure 5 - SensitivityAnalysisAllocationPrincipleOnlyCB.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500) # Save the figure as a PNG file
print(SensitivityAnalysisAllocationPrincipleOnlyCB) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 6 on the sensitivity to the assumed allocation principle (only EAP)
SensitivityAnalysisAllocationPrincipleOnlyEAP <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph != factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                                labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = AllocationPrincipleEAP), position = "dodge") +
  geom_rect(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                                labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white") +
  geom_text(size = 3, data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP != "Equal cumulative per capita (main case)" & CarbonDebtAssumption == 1990 & Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                  labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = ifelse(AllocationPrincipleEAP == "Grandfathering", .5, 0) +
                            ifelse(AllocationPrincipleEAP == "Equal cumulative per capita (whole period)", 1.5, 0) +
                            ifelse(AllocationPrincipleEAP == "Contraction and convergence", 2.5, 0) +
                            ifelse(AllocationPrincipleEAP == "Capability", 3.5, 0), y = Country, label = ifelse(Change == Inf, "+", ifelse(Change == -Inf, "-", paste0(round(Change, 0),"%"))), color = AllocationPrincipleEAP)) +
  geom_hline(yintercept = seq(0.5, 38,1), linewidth = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 250, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 800, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 4, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  geom_vline(data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                        labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
             mapping = aes(xintercept = Change), linetype = "dashed") +
  geom_text(size = 2.5, data = subset(DataFiguresSensitivity, Country == "World" & TempTarget == 1.5 & CarbonDebtAssumption == 1990 & AllocationPrincipleCB == "Equal cumulative per capita (main case)" & AllocationPrincipleEAP == "Equal cumulative per capita (main case)" & Graph == factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                                                                                                                                                                                                   labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
            mapping = aes(x = Change, y = "India", label = "World average", angle = -90, vjust = -.5)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 6, limits = c(0,NA)) +
  facetted_pos_scales(x = list(Graph == factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))) ~ scale_x_continuous(breaks = -1, limits = c(0,NA)))) +
  scale_fill_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[1], scico(4, palette = "roma")[3:5]),
                    labels = c("Capability\n(only for the emission allowances pool\n- based on cumulative GDP over the\nperiod 2023-2070)", 
                               "Main case:\nEqual cumulative emissions per capita", 
                               "Contraction and convergence\n(only for the emission allowances pool\n- based on cumulative population over the period\n2023-2070 and total excessive carbon claims)", 
                               "Equal cumulative emissions per capita\n(only for the emission allowances pool\n- based on cumulative population\nover the period 2023-2070)", 
                               "Grandfathering\n(only for the emission allowances pool\n- based on total excessive carbon\nclaims)")) +
  scale_color_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[3:5]),
                     guide = "none") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                                  strip.background = element_blank(),
                                  strip.placement = "outside",
                                  panel.grid.major.y = element_blank())

png(filename = "Supplementary Figure 6 - SensitivityAnalysisAllocationPrincipleOnlyEAP.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500) # Save the figure as a PNG file
print(SensitivityAnalysisAllocationPrincipleOnlyEAP) # Print the figure
dev.off() # Close the PNG file

# Plot Supplementary Figure 7 comparing carbon debt for territorial and consumption-based emissions
GraphComparingCarbonDebts <- ggplot() +
  geom_col(data = CarbonDebtConsumptionSensitivity, mapping = aes(y= Country, x = CarbonDebt/1000, fill = Accounting), position = position_dodge()) +
  geom_text(size = 3, data = subset(CarbonDebtConsumptionSensitivity, Accounting == "Consumption Emissions"), mapping = aes(y = Country, x = CarbonDebt/1000 + ifelse(CarbonDebt>0, 15, -15), label = paste0(round(ChangeTerrToCons,2)*100, "%"))) +
  scale_x_continuous(breaks = seq(-150, 150, 50)) +
  scale_fill_scico_d(labels = c("Consumption-based", "Territorial")) +
  labs(y = NULL, x = expression(paste("Carbon Debt (Gt", CO[2],")")), fill = "Accounting Framework") +
  theme_bw(base_size = 8) + theme(legend.position = "bottom")

png(filename = "Supplementary Figure 7 - ComparingCarbonDebtTerritorialConsumptionBased.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500) # Save the figure as a PNG file
print(GraphComparingCarbonDebts) # Print the figure
dev.off() # Close the PNG file
  
