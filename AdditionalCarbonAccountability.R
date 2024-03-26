library(xlsx)
library(reshape2)
library(ggplot2)
library(wbstats)
library(jsonlite)
library(httr)
library(doParallel)
library(ggrepel)
library(ggforce)
library(scico)

rm(list = ls(all.names = TRUE))
gc()

setwd("~/Library/CloudStorage/OneDrive-Chalmers/FairShareforSweden/RScripts")
CountryAssumptions <- subset(read.xlsx("CountryAssumptions.xlsx", sheetIndex = 1, colIndex = 1:6), Country != "Sweden")
NoCores <- detectCores() - 1
CarbonBudget = data.frame(TempTarget = c(1.5,2),
                          BudgetGtCO2 = c(250, 800)-25) 

#### Read data from Global Carbon Project and WorldBank ####
DataWorldBank <- wb_data(c(GDP = "NY.GDP.MKTP.KD", GDPpc = "NY.GDP.PCAP.KD"), country = "all")
colnames(DataWorldBank)[which(names(DataWorldBank) == "date")] <- "Year"
colnames(DataWorldBank)[which(names(DataWorldBank) == "country")] <- "Country"
DataWorldBank$Country[DataWorldBank$iso3c == "EGY"] <- "Egypt"
DataWorldBank$Country[DataWorldBank$iso3c == "GMB"] <- "Gambia"
DataWorldBank$Country[DataWorldBank$iso3c == "IRN"] <- "Iran"
DataWorldBank$Country[DataWorldBank$iso3c == "RUS"] <- "Russia"
DataWorldBank$Country[DataWorldBank$iso3c == "KOR"] <- "South Korea"
DataWorldBank$Country[DataWorldBank$iso3c == "TUR"] <- "Türkiye"

CountryAssumptions <- merge(CountryAssumptions, subset(DataWorldBank, Country %in% CountryAssumptions$Country & Year == 2022, select = c(Country,iso3c)), by = "Country", all.x = TRUE)

GCBDataFile <- tempfile(fileext = ".xlsx")
download.file("https://globalcarbonbudgetdata.org/downloads/latest-data/National_Fossil_Carbon_Emissions_2023v1.0.xlsx", destfile = GCBDataFile, mode = "wb")
DataGlobalCarbonBudget <- melt(merge(melt(data = read.xlsx(GCBDataFile, sheetName = "Territorial Emissions", startRow = 12), id = "NA.", value.name = "Territorial Emissions", variable.name = "Country"),
                                     melt(data = read.xlsx(GCBDataFile, sheetName = "Consumption Emissions", startRow = 9), id = "NA.", value.name = "Consumption Emissions", variable.name = "Country"), by = c("Country", "NA."), all.x = TRUE), id = c("NA.", "Country"), value.name = "EmissionsMtCO2", variable.name = "Accounting")
colnames(DataGlobalCarbonBudget)[which(names(DataGlobalCarbonBudget) == "NA.")] <- "Year"
DataGlobalCarbonBudget$EmissionsMtCO2 <- DataGlobalCarbonBudget$EmissionsMtCO2 * 44/12
DataGlobalCarbonBudget$Country <- as.character(DataGlobalCarbonBudget$Country)
DataGlobalCarbonBudget$Country <- gsub(".", " ", DataGlobalCarbonBudget$Country, fixed = TRUE)
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "USA"] <- "United States"
DataGlobalCarbonBudget$Country[DataGlobalCarbonBudget$Country == "EU27"] <- "European Union"
DataGlobalCarbonBudget <- merge(DataGlobalCarbonBudget, subset(CountryAssumptions, select = c(Country, iso3c)), by = "Country")

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

EUStatesISO2 <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE")
#DataUNPopulation <- 
#  foreach(i = UNLocations$id[UNLocations$iso3 %in% CountryAssumptions$iso3c | UNLocations$iso2 %in% EUStatesISO2], .combine = "rbind") %do% {
#    subset(callAPI(paste0("/data/indicators/49/locations/",i,"/start/1960/end/2070")), sexId == 3 & variantId == 4, select = c(location, iso3, iso2, timeLabel, value))
#  }
#https://population.un.org/dataportal/data/indicators/49/locations/32,36,40,56,76,100,124,152,156,170,188,191,196,203,208,231,233,246,250,270,276,348,356,360,364,372,380,392,398,404,410,428,440,442,470,484,504,528,554,566,578,604,608,616,620,642,643,682,702,703,704,705,710,724,752,756,764,784,792,818,826,840,900/start/1960/end/2070/table/pivotbylocation?df=1f046dc3-8d29-4199-b85a-b024caa951e3

DataUNPopulation <- subset(read.csv("unpopulation_dataportal.csv"), SexId == 3 & VariantId == 4, select = c(Location, Iso3, Iso2, Time, Value))

DataUNPopulation <- rbind(DataUNPopulation, cbind(data.frame(Location = rep("European Union", length(1960:2070)),
                                   Iso3 = rep("EUU", length(1960:2070)),
                                   Iso2 = rep("EU", length(1960:2070)),
                                   aggregate(Value ~ Time, subset(DataUNPopulation, Iso2 %in% EUStatesISO2),sum))))
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Location")] <- "Country"
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Time")] <- "Year"
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Value")] <- "Population"
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso2")] <- "iso2c"
colnames(DataUNPopulation)[which(names(DataUNPopulation) == "Iso3")] <- "iso3c"
DataUNPopulation$Year <- as.numeric(DataUNPopulation$Year)
DataUNPopulation <- merge(subset(DataUNPopulation, select = -Country), subset(CountryAssumptions, select = c(Country, iso3c)), by = "iso3c")

DataSSPFutureGDP_data <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926))
DataSSPFutureGDP <- rbind(read.xlsx("iamc_db_GDP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5)), read.xlsx("iamc_db_POP.xlsx", sheetIndex = 1, endRow = 926, colIndex = 1:5, colClasses=rep("character",5)))
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
DataSSPFutureGDP <- melt(DataSSPFutureGDP, measure.vars = as.character(2010:2100), value.name = "Value", variable.name = "Year")
DataSSPFutureGDP$Year <- as.character(DataSSPFutureGDP$Year)
DataSSPFutureGDP$Year <- as.numeric(DataSSPFutureGDP$Year)
DataSSPFutureGDP$Region[DataSSPFutureGDP$Region == "World"] <- "WLD"
DataSSPFutureGDP <- rbind(DataSSPFutureGDP, cbind(Model = "Aggregate", Region = "EUU",
  aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% DataWorldBank$iso3c[DataWorldBank$iso2c %in% EUStatesISO2]), sum)))

#### Preparation for analysis ####
CountryAssumptions$iso3c[CountryAssumptions$Country == "Rest of world"] <- "ROW" 
DataUNPopulation <- rbind(subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c),
                          cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", aggregate(Population ~ Year, subset(DataUNPopulation, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum)))
DataGlobalCarbonBudget <- rbind(subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country & Accounting == "Territorial Emissions" & Year >= 1960, select = -Accounting),
                                cbind(Country = "Rest of world", iso3c = "ROW", aggregate(EmissionsMtCO2 ~ Year, subset(DataGlobalCarbonBudget, Country %in% CountryAssumptions$Country & Accounting == "Territorial Emissions" & Year >= 1960 & Country != "World", select = -Accounting), sum)))
DataWorldBank <- rbind(subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c),
                       cbind(Country = "Rest of world", iso3c = "ROW", iso2c = "RW", merge(aggregate(GDP ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum), aggregate(GDPpc ~ Year, subset(DataWorldBank, iso3c %in% CountryAssumptions$iso3c & Country != "World"), sum), by = "Year")))
DataSSPFutureGDP <- rbind(subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c),
                          cbind(Model = "Aggregate", Region = "ROW", aggregate(Value ~ Scenario+Year+Variable+Unit, subset(DataSSPFutureGDP, Region %in% CountryAssumptions$iso3c & Region != "WLD"), sum)))

DataUNPopulation <- DataUNPopulation[order(DataUNPopulation$Country, DataUNPopulation$Year),]
DataGlobalCarbonBudget <-DataGlobalCarbonBudget[order(DataGlobalCarbonBudget$Country, DataGlobalCarbonBudget$Year),]
DataWorldBank <- DataWorldBank[order(DataWorldBank$Country, DataWorldBank$Year),]
DataSSPFutureGDP <- DataSSPFutureGDP[order(DataSSPFutureGDP$Region, DataSSPFutureGDP$Year),]

DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"] <- DataUNPopulation$Population[DataUNPopulation$Country == "World"] - DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world"]
DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world"] <- DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World"] - DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "Rest of world"]
DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "World"] - DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"]
DataWorldBank$GDPpc[DataWorldBank$Country == "Rest of world"] <- DataWorldBank$GDP[DataWorldBank$Country == "Rest of world"] / DataUNPopulation$Population[DataUNPopulation$Country == "Rest of world" & DataUNPopulation$Year <= 2022]
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "GDP|PPP"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "GDP|PPP"]
DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"] <- DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "WLD" & DataSSPFutureGDP$Variable == "Population"] - DataSSPFutureGDP$Value[DataSSPFutureGDP$Region == "ROW" & DataSSPFutureGDP$Variable == "Population"]

AnnualCapability <- function(Country, TempTarget) {
  AnnualBudget <- c()
  for (Year in 2023:2070) {
    AnnualBudget = c(AnnualBudget,GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] *
      (DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
         DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"]) /
      sum(DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region != "WLD" & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "Population"]^2 / 
            DataSSPFutureGDP$Value[DataSSPFutureGDP$Scenario == "SSP2" & DataSSPFutureGDP$Region != "WLD" & DataSSPFutureGDP$Year == Year & DataSSPFutureGDP$Variable == "GDP|PPP"]))
  }
  TotalBudget = sum(AnnualBudget)
  return(TotalBudget)
}

AnnualPerCapitaConvergence <- function(Country, TempTarget, ConvYear) {
  AnnualBudget <- c()
  for (Year in 2023:2070) {
    AnnualBudget = c(AnnualBudget, GlobalEmissionCurves$EmissionsMtCO2[GlobalEmissionCurves$TempTarget == TempTarget & GlobalEmissionCurves$Year == Year] *
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

#### Calculations ####
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

GlobalEmissionCurves <-
  foreach(TempTarget = c(1.5, 2), .combine = "rbind") %:%
  foreach(Year = 2023:2070, .combine = "rbind") %do% {
    GlobalNetZero = ceiling(CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget]*1000/DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022]*2+2022)
    data.frame(TempTarget = TempTarget, Year = Year,
               EmissionsMtCO2 = if (Year < GlobalNetZero) {approx(y = c(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022],0),
                                                                  x = c(2022,GlobalNetZero), xout = Year, method = "linear")$y} else {0})
  }

Cluster <- makeCluster(NoCores)
registerDoParallel(Cluster)
AdditionalCarbonAccountability <-
  foreach(TempTarget = c(1.5, 2), .combine = "rbind") %:%
  foreach(AllocationPrinciple = c("Carbon debt and Equality", "Equal cumulative per capita", "Grandfathering", "Capability", "Contraction and convergence"), .combine = "rbind") %:%
  foreach(CarbonDebtAssumption = c(1990, 1960, 1980, 2000, FALSE), .combine = "rbind") %dopar% {
    AdditionalCarbonAccountability = data.frame()
    for (Country in CountryAssumptions$Country) {
      CarbonDebt = if (CarbonDebtAssumption != FALSE & AllocationPrinciple != "Equal cumulative per capita") {
        sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= CarbonDebtAssumption]) -
          sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= CarbonDebtAssumption]) * 
          sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year >= CarbonDebtAssumption & DataUNPopulation$Year <= 2022]) /
          sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year >= CarbonDebtAssumption & DataUNPopulation$Year <= 2022])
      } else {0}
      NationalFutureCarbonBudget = 
        if (AllocationPrinciple == "Carbon debt and Equality") {
          CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 * 
            sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year > 2022]) /
            sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year > 2022])
        } else if (AllocationPrinciple == "Equal cumulative per capita") {
          (CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 + sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)])) *
            sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == Country] & DataUNPopulation$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)]) /
            sum(DataUNPopulation$Population[DataUNPopulation$iso3c == CountryAssumptions$iso3c[CountryAssumptions$Country == "World"] & DataUNPopulation$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)])
        } else if (AllocationPrinciple == "Grandfathering") {
          CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == TempTarget] * 1000 * 
            DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022] /
            DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022]
        } else if (AllocationPrinciple == "Capability") {
          AnnualCapability(Country,TempTarget)
        } else if (AllocationPrinciple == "Contraction and convergence") {
          AnnualPerCapitaConvergence(Country, TempTarget, 2040)
        }
      ExcessiveCarbonClaims = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2), sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])) + ifelse(AllocationPrinciple == "Equal cumulative per capita",sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= ifelse(CarbonDebtAssumption == FALSE, 2023, CarbonDebtAssumption)]),0) - NationalFutureCarbonBudget
      TotalExcessiveCarbonClaims = CarbonDebt + ExcessiveCarbonClaims
      AdditionalCarbonAccountability = rbind(AdditionalCarbonAccountability, data.frame(Country = Country, EconomicDevelopment = CountryAssumptions$Development[CountryAssumptions$Country == Country], TempTarget = TempTarget, AllocationPrinciple = AllocationPrinciple, CarbonDebtAssumption = CarbonDebtAssumption, GDPPerCapita = DataWorldBank$GDPpc[DataWorldBank$Year == 2022 & DataWorldBank$Country == Country], CarbonDebt = CarbonDebt, FutureCarbonClaims = sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country]), ExcessiveCarbonClaims = ExcessiveCarbonClaims, TotalExcessiveCarbonClaims = TotalExcessiveCarbonClaims))
    }
    GlobalTotalExcessiveCarbonClaims = sum(AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$TotalExcessiveCarbonClaims > 0 & AdditionalCarbonAccountability$Country != "World"])
    AdditionalCarbonAccountability = cbind(AdditionalCarbonAccountability, AdditionalCarbonAccountability = 0, AdditionalCarbonAccountabilityPerCapita = 0)
    for (Country in CountryAssumptions$Country) {
      if (AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country == Country] < 0 & Country != "World") {
        AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country == Country] = 0
        AdditionalCarbonAccountability$AdditionalCarbonAccountabilityPerCapita[AdditionalCarbonAccountability$Country == Country] = 0
        #AdditionalCarbonAccountability$AllocationRemainingAccountability[AdditionalCarbonAccountability$Country == Country] = sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])
      } else if (AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country == Country] > 0 & Country != "World") {
        AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country == Country] = 
          AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country == Country] * sum(AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country != "World"]) / GlobalTotalExcessiveCarbonClaims
        AdditionalCarbonAccountability$AdditionalCarbonAccountabilityPerCapita[AdditionalCarbonAccountability$Country == Country] = 
          AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country == Country] * sum(AdditionalCarbonAccountability$TotalExcessiveCarbonClaims[AdditionalCarbonAccountability$Country != "World"]) / GlobalTotalExcessiveCarbonClaims / mean(DataUNPopulation$Population[DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070 & DataUNPopulation$Country == Country]) * 1e6
        #AdditionalCarbonAccountability$AllocationRemainingAccountability[AdditionalCarbonAccountability$Country == Country] = sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country]) + AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country == Country]
      } else if (Country == "World") {
        AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country == Country] = sum(AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country != "World"])
        AdditionalCarbonAccountability$AdditionalCarbonAccountabilityPerCapita[AdditionalCarbonAccountability$Country == Country] = sum(AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country != "World"]) / mean(DataUNPopulation$Population[DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070 & DataUNPopulation$Country == Country]) * 1e6
        #AdditionalCarbonAccountability$AllocationRemainingAccountability[AdditionalCarbonAccountability$Country == Country] = sum(PlannedEmissions$EmissionsMtCO2) + sum(AdditionalCarbonAccountability$AdditionalCarbonAccountability[AdditionalCarbonAccountability$Country != "World"])
      }
    }
    data.frame(AdditionalCarbonAccountability)
  }
stopCluster(Cluster)
rm(Cluster) 

#### Figures and tables ####
DataAnnualEmissionsFigure <- cbind(PastFuture = "", merge(subset(rbind(DataGlobalCarbonBudget, PlannedEmissions), !Country %in% c("World", "Rest of world") & Year >= 2000), subset(DataUNPopulation, !Country %in% c("World", "Rest of world") & Year >= 2000), by = c("Country", "Year")))
DataAnnualEmissionsFigure <- merge(DataAnnualEmissionsFigure, subset(CountryAssumptions, select = c(Country, Development)), by = "Country")
DataAnnualEmissionsFigure$PastFuture[DataAnnualEmissionsFigure$Year > 2022] = "Planned emissions"
DataAnnualEmissionsFigure$PastFuture[DataAnnualEmissionsFigure$Year <= 2022] = "Historic emissions"
DataAnnualEmissionsFigure <- rbind(DataAnnualEmissionsFigure, cbind(PastFuture = "Planned emissions", subset(DataAnnualEmissionsFigure, Year == 2022, select = -PastFuture)))
DataAnnualEmissionsFigure$Development <- factor(DataAnnualEmissionsFigure$Development, levels = c("High", "Upper-middle", "Lower-middle", "Low"), labels = c("High\nincome", "Upper-middle\nincome", "Lower-middle\nincome", "Low\nincome"))
CountriesPlannedEmissionsFig <- c("United States", "European Union", "Brazil", "Russia", "India", "China", "South Africa", "Iran", "United Arab Emirates", "Ethiopia")

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
  scale_color_manual(values = scico(5, palette = "batlow")[1:4]) +
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

png(filename = "Figure 1 - FigurePlannedEmissions.png", width = 88, height = 120,  units = "mm", res = 500)
print(FigurePlannedEmissions)
dev.off()

pdf(file = "Figure 1 - FigurePlannedEmissions.pdf", width = 88/25.4, height = 120/25.4)
print(FigurePlannedEmissions)
dev.off()

TableForManuscript <- subset(AdditionalCarbonAccountability, AllocationPrinciple == "Carbon debt and Equality" & CarbonDebtAssumption == 1990 & !Country %in% c("World" ,"Rest of world"))
TableForManuscript <- TableForManuscript[order(TableForManuscript$TempTarget, -TableForManuscript$GDPPerCapita),]
TableForManuscript <- rbind(TableForManuscript, subset(AdditionalCarbonAccountability, AllocationPrinciple == "Carbon debt and Equality" & CarbonDebtAssumption == 1990 & Country %in% c("World" ,"Rest of world")))

TableDataForExcel <- data.frame()
for (Country in CountryAssumptions$Country) {
  TableDataForExcel <- rbind(TableDataForExcel, data.frame(
    Country = Country,
    Population2022 = DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year == 2022],
    AggregatedPopulation19902022 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]),
    AggregatedPopulation20232070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]),
    AggregatedPopulation19902070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]),
    ShareinWorldPopulation2022 = DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year == 2022] / DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year == 2022],
    ShareinWorldPopulation19902022 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2022]),
    ShareinWorldPopulation20232070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 2023 & DataUNPopulation$Year <= 2070]),
    ShareinWorldPopulation19902070 = sum(DataUNPopulation$Population[DataUNPopulation$Country == Country & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]) / sum(DataUNPopulation$Population[DataUNPopulation$Country == "World" & DataUNPopulation$Year >= 1990 & DataUNPopulation$Year <= 2070]),
    NetZeroYear = CountryAssumptions$NetZeroYear[CountryAssumptions$Country == Country],
    HistoricEmissions2022MtCO2 = DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022],
    HistoricEmissions19902022MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]),
    PlannedEmissions20232070MtCO2 = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])),
    TotalEmissions19902070MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) + ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])),
    ShareinWorldEmissions2022MtCO2 = DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year == 2022] / DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year == 2022],
    ShareinWorldEmissions19902022MtCO2 = sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) / sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= 1990]),
    ShareinWorldEmissions20232070MtCO2 = ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country])) / sum(PlannedEmissions$EmissionsMtCO2),
    ShareinWorldEmissions19902070MtCO2 = (sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == Country & DataGlobalCarbonBudget$Year >= 1990]) + ifelse(Country == "World", sum(PlannedEmissions$EmissionsMtCO2),sum(PlannedEmissions$EmissionsMtCO2[PlannedEmissions$Country == Country]))) / (sum(DataGlobalCarbonBudget$EmissionsMtCO2[DataGlobalCarbonBudget$Country == "World" & DataGlobalCarbonBudget$Year >= 1990]) + sum(PlannedEmissions$EmissionsMtCO2))
  ))
}

XlsxRepository <- loadWorkbook("TableForManuscript.xlsx")
removeSheet(XlsxRepository, sheetName = "ResultsFromR-1.5C")
removeSheet(XlsxRepository, sheetName = "ResultsFromR-2C")
removeSheet(XlsxRepository, sheetName = "Data")
saveWorkbook(XlsxRepository, "TableForManuscript.xlsx")

write.xlsx(subset(TableForManuscript, TempTarget == 1.5), "TableForManuscript.xlsx", sheetName = "ResultsFromR-1.5C", row.names = FALSE, append = TRUE)
write.xlsx(subset(TableForManuscript, TempTarget == 2), "TableForManuscript.xlsx", sheetName = "ResultsFromR-2C", row.names = FALSE, append = TRUE)
write.xlsx(TableDataForExcel, "TableForManuscript.xlsx", sheetName = "Data", row.names = FALSE, append = TRUE)

TablewithPlannedEmissions <- dcast(PlannedEmissions, Country ~ Year, value.var = "EmissionsMtCO2")

write.xlsx(TablewithPlannedEmissions, "PlannedEmissions.xlsx", sheetName = "EmissionsMtCO2", row.names = FALSE)

#### Figures Excessive Carbon Claims vs. Carbon Debt ####
DataForZoomFigure <- subset(AdditionalCarbonAccountability, AllocationPrinciple == "Carbon debt and Equality" & Country != "World" & CarbonDebtAssumption == 1990)
DataForZoomFigureLabels <- cbind(DataForZoomFigure, zoom = TRUE)
ZoomLevel <- 22000
DataForZoomFigureLabels$zoom <- ifelse(DataForZoomFigureLabels$CarbonDebt > -ZoomLevel & DataForZoomFigureLabels$CarbonDebt < ZoomLevel & DataForZoomFigureLabels$ExcessiveCarbonClaims > -ZoomLevel & DataForZoomFigureLabels$ExcessiveCarbonClaims < ZoomLevel, TRUE, FALSE)
DataForZoomFigure$EconomicDevelopment <- factor(DataForZoomFigure$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world"))
DataForZoomFigureLabels$EconomicDevelopment <- factor(DataForZoomFigureLabels$EconomicDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low", "Rest of world"), labels = c("High income", "Upper-middle income", "Lower-middle income", "Low income", "Rest of world"))

CarbonDebtvsExcessiveClaims <- ggplot(mapping = aes(x = CarbonDebt/1000, y = ExcessiveCarbonClaims/1000, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_point(data = subset(DataForZoomFigure, TempTarget == 1.5), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, TempTarget == 1.5 & !(Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world"))), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, TempTarget == 1.5 & Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world")), direction = "y", max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-22,-120), y = c(-22, -20), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevel, ZoomLevel)/1000, ylim = c(-ZoomLevel,ZoomLevel)/1000, zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(5, palette = "batlow")[1:4], "gray")) +
  labs(x = expression(paste("Carbon Debt (Gt", CO[2],")")), y = expression(paste("Excessive Carbon Claims (Gt", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Figure 2 - CarbonDebtvsExcessiveClaims.png", width = 88*2, height = 88,  units = "mm", res = 500)
print(CarbonDebtvsExcessiveClaims)
dev.off()

pdf(file = "Figure 2 - CarbonDebtvsExcessiveClaims.pdf", width = 88*2/25.4, height = 88/25.4)
print(CarbonDebtvsExcessiveClaims)
dev.off()

CarbonDebtvsExcessiveClaims2C <- ggplot(mapping = aes(x = CarbonDebt/1000, y = ExcessiveCarbonClaims/1000, color = EconomicDevelopment, label = Country)) +#, alpha = Target, shape = Target)) +
  geom_point(data = subset(DataForZoomFigure, TempTarget == 2), size = .3) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, TempTarget == 2 & !(Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world"))), max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text_repel(data = subset(DataForZoomFigureLabels,  TempTarget == 2 & Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world")), direction = "y", max.overlaps = 40, size = 1.7, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE) +
  geom_text(size = 2.5, color = "black", data = data.frame(x = c(-22,-120), y = c(-22, -120), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevel, ZoomLevel)/1000, ylim = c(-ZoomLevel,ZoomLevel)/1000, zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(5, palette = "batlow")[1:4], "gray")) +
  #scale_alpha_manual(values = c(1,0.4)) +
  labs(x = expression(paste("Carbon Debt (Gt", CO[2],")")), y = expression(paste("Excessive Carbon Claims (Gt", CO[2],")")), color = NULL) +
  theme_light(base_size = 8) + theme(legend.position = c(0.535,0.1),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank())

png(filename = "Extended Data Figure 1 - CarbonDebtvsExcessiveClaims 2C.png", width = 88*2, height = 88,  units = "mm", res = 500)
print(CarbonDebtvsExcessiveClaims2C)
dev.off()

CarbonDebtvsExcessiveClaimsComparison <- ggplot(mapping = aes(x = CarbonDebt/1000, y = ExcessiveCarbonClaims/1000, color = EconomicDevelopment, label = Country, shape = as.character(TempTarget), alpha = as.character(TempTarget))) +
  geom_point(data = subset(DataForZoomFigure), size = .5) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, !(Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world"))), max.overlaps = 40, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE, mapping = aes(size = as.character(TempTarget))) +
  geom_text_repel(data = subset(DataForZoomFigureLabels, Country %in% c("Japan", "European Union", "Russia", "India", "China", "Rest of world")), direction = "y", max.overlaps = 40, segment.size = .3, box.padding = 0.15, min.segment.length = 0.01, show.legend = FALSE , mapping = aes(size = as.character(TempTarget))) +
  geom_text(size = 2.5, color = "black", alpha = 1, data = data.frame(x = c(-22,-120), y = c(-22, -120), zoom = c(TRUE,FALSE), label = c("a)", "b)")),
            mapping = aes(x = x, y = y, label = label)) +
  facet_zoom(xlim = c(-ZoomLevel, ZoomLevel)/1000, ylim = c(-ZoomLevel,ZoomLevel)/1000, zoom.data = zoom, horizontal = TRUE) +
  scale_x_continuous(breaks = pretty) +
  scale_y_continuous(breaks = pretty) +
  scale_color_manual(values = c(scico(5, palette = "batlow")[1:4], "gray")) +
  scale_size_manual(values = c(1.7,1), labels = c("1.5°C target", "2°C target")) +
  scale_alpha_manual(values = c(1,0.4), labels = c("1.5°C target", "2°C target")) +
  scale_shape_discrete(labels = c("1.5°C target", "2°C target")) +
  labs(x = expression(paste("Carbon Debt (Gt", CO[2],")")), y = expression(paste("Excessive Carbon Claims (Gt", CO[2],")")), color = NULL, shape = NULL, size = NULL, alpha = NULL) +
  theme_light(base_size = 8) + theme(legend.position = c(0.535,0.12),
                                     legend.key.size = unit(1, "mm"),
                                     legend.background = element_blank(),
                                     legend.margin = margin(0),
                                     legend.box.margin = margin(0),
                                     legend.spacing = unit(0,"pt"))

png(filename = "Extended Data Figure 2 - CarbonDebtvsExcessiveClaims Comparing 1.5C and 2C.png", width = 88*2, height = 88,  units = "mm", res = 500)
print(CarbonDebtvsExcessiveClaimsComparison)
dev.off()

#### Figures and Data for Sensitivity Analysis ####
DataFiguresSensitivity <- cbind(AdditionalCarbonAccountability, ChangeComparedToMain = 0, ChangeComparedToMainPerCapita = 0, TotExcessiveChangeToMain = 0)
DataFiguresSensitivity <- DataFiguresSensitivity[order(DataFiguresSensitivity$TempTarget, DataFiguresSensitivity$AllocationPrinciple, DataFiguresSensitivity$CarbonDebtAssumption, DataFiguresSensitivity$Country),]
for (i in unique(DataFiguresSensitivity$AllocationPrinciple)) {
  for (j in unique(DataFiguresSensitivity$CarbonDebtAssumption)) {
    DataFiguresSensitivity$ChangeComparedToMain[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$AdditionalCarbonAccountability[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$AdditionalCarbonAccountability[DataFiguresSensitivity$AllocationPrinciple == "Carbon debt and Equality" & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
    DataFiguresSensitivity$ChangeComparedToMainPerCapita[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$AdditionalCarbonAccountabilityPerCapita[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$AdditionalCarbonAccountabilityPerCapita[DataFiguresSensitivity$AllocationPrinciple == "Carbon debt and Equality" & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
    DataFiguresSensitivity$TotExcessiveChangeToMain[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] <- 100*(DataFiguresSensitivity$TotalExcessiveCarbonClaims[DataFiguresSensitivity$AllocationPrinciple == i & DataFiguresSensitivity$CarbonDebtAssumption == j] / DataFiguresSensitivity$TotalExcessiveCarbonClaims[DataFiguresSensitivity$AllocationPrinciple == "Carbon debt and Equality" & DataFiguresSensitivity$CarbonDebtAssumption == 1990] - 1)
  }
}
DataFiguresSensitivity <- DataFiguresSensitivity[order(DataFiguresSensitivity$TempTarget, DataFiguresSensitivity$AllocationPrinciple, DataFiguresSensitivity$CarbonDebtAssumption, DataFiguresSensitivity$EconomicDevelopment), ]
CountriesForSensitivity = subset(DataWorldBank, Year == 2022, select = c(Country,GDPpc))
CountriesForSensitivity <- CountriesForSensitivity[order(CountriesForSensitivity$GDPpc), ]
DataFiguresSensitivity$Country <- factor(DataFiguresSensitivity$Country, levels = CountriesForSensitivity$Country)
DataFiguresSensitivity$AdditionalCarbonAccountability <- DataFiguresSensitivity$AdditionalCarbonAccountability/1e3

write.xlsx(DataFiguresSensitivity, "DataForSensitivityAnalysis.xlsx", row.names = FALSE)

DataFiguresSensitivity <- melt(DataFiguresSensitivity, measure.vars = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), variable.name = "Graph", value.name = "Change")
DataFiguresSensitivity$Graph <- factor(DataFiguresSensitivity$Graph, levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                       labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))

SensitivityAnalysisCarbonDebtAssumption <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & (CarbonDebtAssumption != FALSE & AllocationPrinciple == "Carbon debt and Equality")),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = as.character(CarbonDebtAssumption)), position = "dodge") +
  geom_hline(yintercept = seq(0.5, 38,1), size = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 225, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 600, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 75, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous() +
  scale_fill_manual(values = c(scico(4, palette = "roma")[2:3], scico(4, palette = "roma")[1], scico(4, palette = "roma")[4]),
                    labels = c("1960", "1980", "Main case: 1990", "2000")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                     strip.background = element_blank(),
                     strip.placement = "outside",
                     panel.grid.major.y = element_blank())

png(filename = "Extended Data Figure 3 - SensitivityAnalysisCarbonDebtAssumption.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500)
print(SensitivityAnalysisCarbonDebtAssumption)
dev.off()

SensitivityAnalysisAllocationPrinciple <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & ((CarbonDebtAssumption == FALSE & !AllocationPrinciple %in% c("Carbon debt and Equality")) | (CarbonDebtAssumption == 1990 & AllocationPrinciple %in% c("Carbon debt and Equality")))),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = AllocationPrinciple), position = "dodge") +
  geom_hline(yintercept = seq(0.5, 38,1), size = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 250, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 600, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 200, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 6) +
  scale_fill_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[1], scico(4, palette = "roma")[3:5]),
                    labels = c("Capability\n(based on annual GDP per\ncapita 2023-2070)", "Main case:\nEqual cumulative emissions per capita combined\nwith responsibility for carbon debt since 1990", "Contraction and convergence\n(reaching equal annual emissions\nper capita by 2040)", "Equal cumulative\nemissions per capita\n(over the period 2023-2070)", "Grandfathering\n(based on share in\nglobal emissions in 2022")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                     strip.background = element_blank(),
                     strip.placement = "outside",
                     panel.grid.major.y = element_blank())

png(filename = "Extended Data Figure 4 - SensitivityAnalysisAllocationPrinciple.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500)
print(SensitivityAnalysisAllocationPrinciple)
dev.off()

SensitivityAnalysisAllocationPrinciplewCarbonDebt <- ggplot() +
  geom_col(data = subset(DataFiguresSensitivity, !Country %in% c("World", "Rest of world") & TempTarget == 1.5 & ((CarbonDebtAssumption == 1990 & AllocationPrinciple != "Carbon debt and Equality") | (CarbonDebtAssumption == 1990 & AllocationPrinciple == "Carbon debt and Equality"))),
           mapping = aes(x = ifelse(Change == Inf, NA, Change), y = Country, fill = AllocationPrinciple), position = "dodge") +
  geom_hline(yintercept = seq(0.5, 38,1), size = .2, color = "gray") +
  geom_text(data = rbind(data.frame(x = 200, y = 1, label = "a)", Graph = factor("AdditionalCarbonAccountability", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 600, y = 1, label = "b)", Graph = factor("AdditionalCarbonAccountabilityPerCapita", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                               labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)"))))),
                         data.frame(x = 50, y = 1, label = "c)", Graph = factor("ChangeComparedToMain", levels = c("AdditionalCarbonAccountability", "AdditionalCarbonAccountabilityPerCapita", "ChangeComparedToMain"), 
                                                                                 labels = c(expression(paste("Additional carbon accountability (Gt", CO[2],")")), expression(paste("Additional carbon accountability per capita (t", CO[2],")")), expression(paste("Change compared to main case (%)")))))),
            mapping = aes(x = x, y = y, label = label)) +
  facet_wrap(~Graph, scales = "free_x", nrow = 1, strip.position = "bottom", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 6) +
  scale_fill_manual(values = c(scico(5, palette = "roma")[2], scico(4, palette = "roma")[1], scico(4, palette = "roma")[3:5]),
                    labels = c("Capability + Responsibility\n(based on annual GDP per\ncapita 2023-2070, and carbon debt since 1990)", "Main case:\nEqual cumulative emissions per capita combined\nwith responsibility for carbon debt since 1990", "Contraction and convergence + Responsibility\n(reaching equal annual emissions per capita\nby 2040 and carbon debt since 1990)", "Equal cumulative\nemissions per capita\n(over the period 1990-2070)", "Grandfathering + Responsibility\n(based on share in 2022 global\nemissions and carbon debt\nsince 1990)")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw(base_size = 8) + theme(legend.position = "bottom",
                     strip.background = element_blank(),
                     strip.placement = "outside",
                     panel.grid.major.y = element_blank())

png(filename = "Extended Data Figure 5 - SensitivityAnalysisAllocationPrinciplewCarbonDebt.png", width = 88*2, height = 88*2.5,  units = "mm", res = 500)
print(SensitivityAnalysisAllocationPrinciplewCarbonDebt)
dev.off()

