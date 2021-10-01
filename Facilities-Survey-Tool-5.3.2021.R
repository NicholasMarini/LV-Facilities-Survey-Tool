library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(gdata)
library(DT)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(rsconnect)
library(reshape2)
library(anchors)
library(IDPmisc)
library(geosphere)
library(measurements)
library(stringi)
library(scales)
library(googlesheets)
library(googlesheets4)
library(shinyalert)
library(rgdal)
library(sf)
library(stringr)
library(scales)
library(Rcpp)
library(htmlwidgets)
library(htmltools)
library(dendroTools)
library(DescTools)
library(shinyscreenshot)
library(webshot)
library(devtools)
library(leaflegend)
library(bsplus)
library(shinyBS)
library(data.table)


# Data input -----------------------------------------------------------------------------------------

## Reads in and formats data from googlesheets
WorkBook_ID <- "https://docs.google.com/spreadsheets/d/11YT5qLEG1eCVLI9wYabShSEbUia6n5Zg-gjtFREvwyk/edit#gid=1569831357"
googlesheets4::gs4_auth( 
  cache = ".secrets",
  email = "nmarini@kitamba.com")

df <- sheets_read(ss = WorkBook_ID, sheet = "Schools", na = "NA", col_types = ("cccccccccnnccccncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncccnnnnnnnnnn"))
FDf <- sheets_read(ss = WorkBook_ID, sheet = "Facilities.API")
newFacilities <- sheets_read(ss = WorkBook_ID, sheet = "New.Facilities")
ZipCenterPoints <- sheets_read(ss = WorkBook_ID, sheet = "NV.ZipCode.CenterPoints")

## Read in Zip Code & Opportunity Zone shape files (shinyapps deployment requires relative file paths)
ZipLines <- read_sf("www/zip/zipcodes.shp")
OppZones <- read_sf("www/oppzones/opportunity_zones.shp")


## Read in leaflet-easy print screenshot bundle
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"


# Regional Need --------------------------------------------------------------------------------------

## Defines weights for Regional Need factors (quality, utilization, & surplus enrollment)

qFctr<-0.6
uFctr<-0.2
eFctr<-0.2

## Reformat table and rename as schDemand with our ids and measures
schDemand<- melt(df, id.vars = c("HS.Zone.Loc.ID", "HS.Zone.Name","Inactive","Latitude","Longitude", "Zip_Code", "School.Level","Charter.Flag", "School.Quality", "School.Name", "Assembly", "Senate", "Trustee",
                                 "SchoolQuality_ES", "SchoolQuality_MS", "SchoolQuality_HS", "SchoolQuality_Avg", "FRL", "ELL", "IEP",
                                 "Demo_Asian", "Demo_Black", "Demo_White", "Demo_Hispanic", "Demo_AmericanIndian", "Demo_PacificIslander", "Demo_MultiRace"), measure.vars = c("200809.Capacity", "200910.Capacity", "201011.Capacity", "201112.Capacity", 
                                                                                                                                                                               "201213.Capacity", "201314.Capacity", "201415.Capacity", "201516.Capacity", 
                                                                                                                                                                               "201617.Capacity", "201718.Capacity", "201819.Capacity", "201920.Capacity",
                                                                                                                                                                               "202021.Capacity","202122.Capacity", "202223.Capacity", "202324.Capacity",
                                                                                                                                                                               "202425.Capacity", "202526.Capacity","202627.Capacity","202728.Capacity",
                                                                                                                                                                               "200809.Portables", "200910.Portables", "201011.Portables", "201112.Portables",
                                                                                                                                                                               "201213.Portables", "201314.Portables", "201415.Portables", "201516.Portables",
                                                                                                                                                                               "201617.Portables", "201718.Portables", "201819.Portables", "201920.Portables",
                                                                                                                                                                               "202021.Portables","202122.Portables", "202223.Portables", "202324.Portables",
                                                                                                                                                                               "202425.Portables", "202526.Portables","202627.Portables","202728.Portables",
                                                                                                                                                                               "200809.Enrollment", "200910.Enrollment", "201011.Enrollment", "201112.Enrollment",
                                                                                                                                                                               "201213.Enrollment", "201314.Enrollment", "201415.Enrollment", "201516.Enrollment",
                                                                                                                                                                               "201617.Enrollment", "201718.Enrollment", "201819.Enrollment", "201920.Enrollment", 
                                                                                                                                                                               "202021.Enrollment.Projections", "202122.Enrollment.Projections","202223.Enrollment.Projections","202324.Enrollment.Projections",
                                                                                                                                                                               "202425.Enrollment.Projections","202526.Enrollment.Projections", "202627.Enrollment.Projections", "202728.Enrollment.Projections"))

                                                                                                                                                                               
schDemand<-subset(schDemand, Inactive == 0)

colnames(schDemand)[colnames(schDemand)=="HS.Zone.Name"] <- "Demand.Region"

## Create a new variable for Year
schDemand %>% separate(variable, c("Year", "Variable")) -> schDemand
schDemand$value<-as.numeric(schDemand$value)
schDemand$Year<-gsub( "X", "", schDemand$Year)
schDemand$Year<- str_replace(schDemand$Year, "(\\d{4})(\\d{2})", "\\1-\\2")

## Aggregates variables across the measures that are specified
masterSchDemand <- dcast(schDemand, HS.Zone.Loc.ID + Demand.Region + Zip_Code + Latitude + Longitude + School.Level + Charter.Flag + School.Quality + SchoolQuality_ES + SchoolQuality_MS + SchoolQuality_HS + SchoolQuality_Avg + Assembly + Senate + Trustee + FRL + ELL + IEP + Demo_Asian + Demo_Black + Demo_White + Demo_Hispanic + Demo_AmericanIndian + Demo_PacificIslander + Demo_MultiRace + Year ~ Variable, fun.aggregate = sum, na.rm = TRUE)


## Create dataframe to use for map with Year included
schDemandMap <- dcast(schDemand, HS.Zone.Loc.ID + Demand.Region + Zip_Code +  School.Name + Latitude + Longitude + School.Level + Charter.Flag + School.Quality +  SchoolQuality_ES + SchoolQuality_MS + SchoolQuality_HS + SchoolQuality_Avg + Assembly + Senate + Trustee + FRL + ELL + IEP + Demo_Asian + Demo_Black + Demo_White + Demo_Hispanic + Demo_AmericanIndian + Demo_PacificIslander + Demo_MultiRace + Year ~ Variable, fun.aggregate = sum, na.rm = TRUE)

## Create dataframe to use for Zip Codes  / heat map with Year included

ZipCodeDemand <- dcast(schDemand,  Zip_Code  + School.Level  +  SchoolQuality_ES + SchoolQuality_MS + SchoolQuality_HS + SchoolQuality_Avg + FRL + ELL + IEP + Demo_Asian + Demo_Black + Demo_White + Demo_Hispanic + Demo_AmericanIndian + Demo_PacificIslander + Demo_MultiRace + Year ~ Variable, fun.aggregate = sum, na.rm = TRUE)


## Create new variable Utilization for map using enrollment and capacity numbers
schDemandMap<-schDemandMap%>%
  mutate(Utilization = Enrollment / Capacity)

## Get rid of any non-number entries caused by zeros
schDemandMap <- schDemandMap[is.finite(schDemandMap$Utilization),]   

## Create new variable Surplus.Enrollment for map using enrollment and capacity
schDemandMap<-schDemandMap%>%
  mutate(Surplus.Enrollment = Enrollment - Capacity)

## Create new variable Surplus.Enrollment for map using enrollment and capacity
ZipCodeDemand<-ZipCodeDemand%>%
  mutate(Surplus.Enrollment = Enrollment - Capacity)

## Create new variable Utilization for map using enrollment and capacity numbers
ZipCodeDemand<-ZipCodeDemand%>%
  mutate(Utilization = Enrollment / Capacity)

## Get rid of any infinite entries caused by zeros
ZipCodeDemand[sapply(ZipCodeDemand, is.infinite)] <- NA
ZipCodeDemand[sapply(ZipCodeDemand, is.nan)] <- NA

## Find max utilization to be used in utility demand score calculation
schoolUtMax<-aggregate(schDemandMap$Utilization, by=list(Year=schDemandMap$Year, School.Level=schDemandMap$School.Level), FUN = max)
schoolSurMax<-aggregate(schDemandMap$Surplus.Enrollment, by=list(Year=schDemandMap$Year, School.Level=schDemandMap$School.Level), FUN = max)

## merge max data frames with school demand map data frame and rename columns appropriately
schDemandMap<-merge(schDemandMap,schoolUtMax, by=c("School.Level", "Year"),all.schoolUtMax = TRUE)
colnames(schDemandMap)[colnames(schDemandMap)=="x"] <- "Util.Max"

schDemandMap<-merge(schDemandMap,schoolSurMax, by=c("School.Level", "Year"),all.schoolSurMax = TRUE)
colnames(schDemandMap)[colnames(schDemandMap)=="x"] <- "Surp.Max"


## Create new columns for demand utilization, quality, and surplus and then add them to find final demand score 
schDemandMap<-schDemandMap%>%
  mutate(Demand.Score.Utilization = (Utilization/Util.Max)*uFctr)
schDemandMap <-schDemandMap%>%
  mutate(Demand.Score.Quality = (1-School.Quality/5)*qFctr)
schDemandMap <-schDemandMap%>%
  mutate(Demand.Score.Surplus = ifelse(Surplus.Enrollment > 0, (Surplus.Enrollment/Surp.Max)*eFctr, 0))
schDemandMap<-schDemandMap%>%
  mutate(Demand.Score = Demand.Score.Utilization + Demand.Score.Quality + Demand.Score.Surplus)



schDemandMap<-schDemandMap %>% 
  mutate(FRL_Percent = percent(FRL/Enrollment, accuracy = 0.1))
schDemandMap<-schDemandMap %>% 
  mutate(ELL_Percent = percent(ELL/Enrollment, accuracy = 0.1))
schDemandMap<-schDemandMap %>% 
  mutate(IEP_Percent = percent(IEP/Enrollment, accuracy = 0.1))


## Find the total capacity for each zone by grouping and summing over zone, year, and level
zoneCap<-aggregate(masterSchDemand$Capacity, by=list(HS.Zone.Loc.ID=masterSchDemand$HS.Zone.Loc.ID, Year=masterSchDemand$Year, School.Level=masterSchDemand$School.Level),FUN = sum, Charter.Flag=0)

## Find the total enrollment for each zone by grouping and summing over zones, year, and level
zoneEnroll<-aggregate(masterSchDemand$Enrollment, by=list(HS.Zone.Loc.ID=masterSchDemand$HS.Zone.Loc.ID,Year=masterSchDemand$Year, School.Level=masterSchDemand$School.Level),FUN = sum, Charter.Flag=0)

## Find the average quality for each zone by grouping and averaging over zone, year, and level
zoneQuality<-aggregate(masterSchDemand$School.Quality, by=list(HS.Zone.Loc.ID=masterSchDemand$HS.Zone.Loc.ID,Year=masterSchDemand$Year, School.Level=masterSchDemand$School.Level),FUN = mean, Charter.Flag=0)


## Merge our data frames to create a data frame with zoneCap, zoneEnroll, and ZoneQuality plus all of the other variables from the masterSchDemand
finalSchDemand<-merge(masterSchDemand,zoneCap, by=c("HS.Zone.Loc.ID", "Year", "School.Level"))
finalSchDemand<-merge(finalSchDemand,zoneEnroll, by=c("HS.Zone.Loc.ID","Year", "School.Level"))
finalSchDemand<-merge(finalSchDemand,zoneQuality, by=c("HS.Zone.Loc.ID","Year", "School.Level"))

## Get rid of any rows that are duplicates acorss Hs.Zone.Loc.ID, Year, School.Level, and Average Quality (x)
finalSchDemand<-distinct(finalSchDemand, HS.Zone.Loc.ID, Year, School.Level,x, .keep_all=TRUE )

## Rename 'x' to 'Avg.School.Quality'
colnames(finalSchDemand)[colnames(finalSchDemand)=="x"] <- "Avg.School.Quality"

## Get rid of unnecessary columns again
finalSchDemand<-subset(finalSchDemand,select = c(HS.Zone.Loc.ID, Demand.Region, Year, School.Level, Avg.School.Quality, Capacity, Enrollment, Portables))


## Create new variable Utilization using enrollment and capacity numbers
finalSchDemand<-finalSchDemand%>%
  mutate(Utilization = Enrollment / Capacity)

## Get rid of any non-number entries caused by zeros
finalSchDemand<-NaRV.omit(finalSchDemand)

## Default selection definitions 
schoolLevel <- "ES"
schoolYear <- "2020-21"
DemographicYear <- "2018-19"

##Function for Zip Code Demand heatmap and Table
## As inputs it takes the zipCodeDemand dataframe (cleaned school dataframe), selected School level and year, the Demographic Year (year of most recent demographic data), and factors for utilization, quality, and surplus enrollment
## Function outputs a dataframe with Demand by Zip Code for the inputs selected. -------------------------
ZipDemand.calculation <- function(ZipCodeDemand, schoolLevel, schoolYear, DemographicYear, uFctr, qFctr, eFctr){
  
  ## Filter the data frame by the selected level and year
  selectedZipDemand <- ZipCodeDemand[which(ZipCodeDemand$School.Level == schoolLevel & ZipCodeDemand$Year == schoolYear), ]
  
  ## Create demographic data frame with all 2018-19 data
  demoframe <- ZipCodeDemand[which(ZipCodeDemand$School.Level == schoolLevel & ZipCodeDemand$Year == DemographicYear),]
  demoframe <- subset(demoframe, select = c("Zip_Code", "FRL", "ELL", "IEP", "Enrollment"))
  demoframe[is.na(demoframe)] = 0
  demoframe <- demoframe %>% 
    mutate(Sch_FRL_Enr = Enrollment * (FRL/100))
  demoframe <- demoframe %>% 
    mutate(Sch_ELL_Enr = Enrollment * (ELL/100))
  demoframe <- demoframe %>% 
    mutate(Sch_IEP_Enr = Enrollment * (IEP/100))
  demoframe <- demoframe %>%
    group_by(Zip_Code) %>%
    mutate(Enrollment = sum(Enrollment))
  demoframe <- demoframe %>%
    group_by(Zip_Code) %>%
    mutate(FRL = sum(Sch_FRL_Enr) / Enrollment)
  demoframe <- unique(demoframe)
  demoframe <- demoframe %>%
    group_by(Zip_Code) %>%
    mutate(ELL = (sum(Sch_ELL_Enr) / Enrollment))
  demoframe <- unique(demoframe)
  demoframe <- demoframe %>%
    group_by(Zip_Code) %>%
    mutate(IEP = (sum(Sch_IEP_Enr) / Enrollment))
  demoframe <- unique(demoframe)
  
  
  ## Transform quality and capacity data
  ZipQuality <- selectedZipDemand
  ZipQuality[is.na(ZipQuality)] = 0
  ZipQuality <- ZipQuality %>%
    group_by(Zip_Code) %>% 
    mutate(weighted_quality = weighted.mean(SchoolQuality_Avg, Enrollment, na.rm = FALSE))
  ZipQuality <- subset(ZipQuality, select = c("Zip_Code", "weighted_quality"))
  ZipQuality <- unique(ZipQuality)
  ZipUtilization <- selectedZipDemand %>%
    group_by(Zip_Code) %>%
    mutate(weighted_utilization = weighted.mean(Utilization, Enrollment, na.rm = TRUE))
  ZipUtilization <- subset(ZipUtilization, select = c("Zip_Code", "weighted_utilization"))
  ZipUtilization[is.na(ZipUtilization)] = 0
  ZipUtilization <- unique(ZipUtilization)
  ZipSurplusEnr <- selectedZipDemand %>%
    group_by(Zip_Code) %>%
    mutate(total.surplus.enrollment = sum(Surplus.Enrollment))
  ZipSurplusEnr <- subset(ZipSurplusEnr, select = c("Zip_Code", "total.surplus.enrollment"))
  ZipSurplusEnr <- unique(ZipSurplusEnr)
  ZipPortables <- selectedZipDemand %>%
    group_by(Zip_Code) %>%
    mutate(total.portables = sum(Portables))
  ZipPortables <- subset(ZipPortables, select = c("Zip_Code", "total.portables"))
  ZipPortables <- unique(ZipPortables)
  ZipCodeDemandfinal <- merge(ZipQuality, ZipUtilization, by = c("Zip_Code"))
  ZipCodeDemandfinal <- merge(ZipCodeDemandfinal, ZipSurplusEnr, by = c("Zip_Code"))
  ZipCodeDemandfinal <- merge(ZipCodeDemandfinal, ZipPortables, by = c("Zip_Code"))
  ZipCodeDemandfinal <- merge(ZipCodeDemandfinal, demoframe, by = c("Zip_Code"))
  ZipCodeDemandfinal <- unique(ZipCodeDemandfinal)
  
  ## Conduct calculations for demand score
  Max.Utilization <- max(ZipCodeDemandfinal$weighted_utilization, na.rm = FALSE)
  ZipSurpMax <- max(ZipCodeDemandfinal$total.surplus.enrollment, na.rm = FALSE)
  ZipCodeDemandfinal <- ZipCodeDemandfinal%>%
    mutate(Demand.Score.Util = (weighted_utilization/Max.Utilization)*uFctr)
  ZipCodeDemandfinal <- ZipCodeDemandfinal%>%
    mutate(Demand.Score.Quality = (1-weighted_quality/5)*qFctr)
  ZipCodeDemandfinal <-ZipCodeDemandfinal%>%
    mutate(Demand.Score.Surplus = ifelse(total.surplus.enrollment > 0, (total.surplus.enrollment/ZipSurpMax)*eFctr, 0))
  ZipCodeDemandfinal <- ZipCodeDemandfinal %>%
    mutate(Demand.Score = Demand.Score.Quality + Demand.Score.Surplus + Demand.Score.Util)
  
  ## Find quantiles and categorize scores as: "very high", "high", "medium", and "low"
  quantiles <- quantile(ZipCodeDemandfinal$Demand.Score, na.rm = FALSE)
  ZipCodeDemandfinal <-ZipCodeDemandfinal%>%
    mutate(Regional.Need = ifelse(Demand.Score < quantiles[2], "Low", ifelse(Demand.Score < quantiles[3], "Moderate", ifelse(Demand.Score < quantiles[4], "High", ifelse (Demand.Score >= quantiles[4], "Very High", NA)))))
  
  ZipCodeDemandfinal <- ZipCodeDemandfinal %>%
    mutate(demandRank = rank(Demand.Score, TRUE))
  
  ## Format table for output subsetting, making percent, and descending order
  ZipDemandOutput <- subset(ZipCodeDemandfinal, select = c("Regional.Need", "Demand.Score", "Zip_Code", "weighted_quality", "weighted_utilization", "total.surplus.enrollment", "total.portables", "FRL", "ELL", "IEP", "demandRank"))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(Demand.Score = percent(Demand.Score, accuracy = 0.1))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(weighted_quality = round(weighted_quality, digits = 2))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(weighted_utilization = percent(weighted_utilization, accuracy = 0.1))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(FRL = percent(FRL, accuracy = 0.1))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(ELL = percent(ELL, accuracy = 0.1))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(IEP = percent(IEP, accuracy = 0.1))
  ZipDemandOutput <- ZipDemandOutput %>%
    mutate(total.surplus.enrollment = ifelse(total.surplus.enrollment> 0, total.surplus.enrollment, 0))
  ZipDemandOutput <- unique(ZipDemandOutput)
  ZipDemandOutput <- ZipDemandOutput %>%
    arrange(desc(demandRank))
  ZipDemandOutput <- subset(ZipDemandOutput, select = c("Regional.Need", "Demand.Score", "Zip_Code", "weighted_quality", "weighted_utilization", "total.surplus.enrollment", "total.portables", "FRL", "IEP", "ELL"))
  
}


## Create new variable Surplus.Enrollment using enrollment and capacity
finalSchDemand<-finalSchDemand%>%
  mutate(Surplus.Enrollment = ifelse(Enrollment > Capacity, Enrollment - Capacity, 0))

## Find the Zone Utility Max by finding the max value for utilization grouped by year and level
zoneUtMax<-aggregate(finalSchDemand$Utilization, by=list(Year=finalSchDemand$Year, School.Level=finalSchDemand$School.Level), FUN = max)

## merge ZoneUtMax into our finalSchDemand table
finalSchDemand<-merge(finalSchDemand,zoneUtMax, by=c("Year", "School.Level"),all.zoneUtMax = TRUE)

## Rename column from x to 'Max.Utilzation'
colnames(finalSchDemand)[colnames(finalSchDemand)=="x"] <- "Max.Utilization"


## Create new variables for demand score utility and demand score quality 
demandScrUt<-finalSchDemand%>%
  mutate(Demand.Score.Utilization = (Utilization/Max.Utilization)*uFctr)
demandScrQ<-finalSchDemand%>%
  mutate(Demand.Score.Quality = (1-Avg.School.Quality/5)*qFctr)

## Find max surplus enrollment and merge back into final school demand table
zoneSurMax<-aggregate(finalSchDemand$Surplus.Enrollment, by=list(Year=finalSchDemand$Year, School.Level=finalSchDemand$School.Level), FUN = max)
finalSchDemand<-merge(finalSchDemand,zoneSurMax, by=c("Year", "School.Level"),all.zoneSurMax = TRUE)

## Rename column for max surplus
colnames(finalSchDemand)[colnames(finalSchDemand)=="x"] <- "Zone.Surp.Max"

## create a dataframe for surplus enrollment
demandScrSur <-finalSchDemand%>%
  mutate(Demand.Score.Surplus = ifelse(Surplus.Enrollment > 0, (Surplus.Enrollment/Zone.Surp.Max)*eFctr, 0))

## Create demand score data frame with all factors to create final demand score
demandScr <- merge(demandScrUt,demandScrQ, by=c("HS.Zone.Loc.ID","Year", "School.Level"),all = TRUE)
demandScr <- merge(demandScr, demandScrSur, by=c("HS.Zone.Loc.ID","Year","School.Level"),all=TRUE)

## Creates demand score using the demand score utility and demand score quality
demandScr<-demandScr%>%
  mutate(Demand.Score = Demand.Score.Utilization + Demand.Score.Quality + Demand.Score.Surplus)

## Remove all unneccessary columns from demandScr
demandScr<-subset(demandScr,select = c(HS.Zone.Loc.ID, Year, School.Level, Demand.Score))

## merge demandScr and finalSchDemand
finalSchDemand<-merge(finalSchDemand,demandScr, by=c("HS.Zone.Loc.ID","Year", "School.Level"), all.demandScr = TRUE)

## Remove unneccesary columns
finalSchDemand<-subset(finalSchDemand,select = c(HS.Zone.Loc.ID, Demand.Region, Year, School.Level, Avg.School.Quality, Utilization, Demand.Score, Surplus.Enrollment, Portables))


# relative to zone code - school ID, School name, Year, school level, latitude, longitude, school quality, capacity, enrollment, school Utilization Rate
## Reorder columns in data frame
finalSchDemand<-finalSchDemand[c("Demand.Score", "Demand.Region", "Year", "School.Level", "Avg.School.Quality", "Utilization", "Surplus.Enrollment", "Portables")]

## Order by demand score (descending)
finalSchDemand<-finalSchDemand[order(-finalSchDemand$Demand.Score),]

#Facilities Supply--------------------------------------------------------------------------------------

## Creates the initial master facilities supply dataframe
facilitySupply<-FDf

facilitySupply$address <- paste(facilitySupply$street_number, facilitySupply$street, facilitySupply$city, facilitySupply$state, facilitySupply$postal_code, sep = " ")


facilitySupply$vacant_land<-gsub( "No", "", facilitySupply$vacant_land)
facilitySupply$vacant_land[is.na(facilitySupply$vacant_land)] <- " "
facilitySupply$lease_term[is.na(facilitySupply$lease_term)] <- " "

## Isolates just the subset of columns/vectors from the dataframe that will displayed or used in server calculations
facilitySupply<-subset(facilitySupply,select = c(listing_id, address, listing_size, listing_rate_per_sqft_per_year, listing_rate_per_year, Latitude, Longitude, listing_url, agent1_name, agent1_email_addresses, agent1_phone, vacant_land, lease_term))

facilitySupply <- rbind(facilitySupply, newFacilities)

facilitySupply<-facilitySupply %>% 
  mutate(Zip_Code = StrRight(address, 5))

## Establishes variables for Max Size and Cost
sizeMax<-max(facilitySupply$listing_size, na.rm=TRUE)
costMax<-max(facilitySupply$listing_rate_per_year, na.rm=TRUE)

## Replaces NAs with zeros to ensure calculations based on size and cost do not result in additional NAs/errors
facilitySupply$listing_size[is.na(facilitySupply$listing_size)]<-0
facilitySupply$listing_rate_per_year[is.na(facilitySupply$listing_rate_per_year)]<- facilitySupply$listing_size * facilitySupply$listing_rate_per_sqft_per_year
facilitySupply$listing_rate_per_year[is.na(facilitySupply$listing_rate_per_year)]<-0

## Regional need map output Creates the facility size supply score factor
facilitySupply<-facilitySupply%>%
  mutate(Supply.Score.Size = ifelse(listing_size > 0, (listing_size/sizeMax), 0))

## Creates the facility cost supply score factor
facilitySupply<-facilitySupply%>%
  mutate(Supply.Score.Cost = ifelse(listing_rate_per_year > 0, (1-(listing_rate_per_year/costMax)), 0))


## Isolates only the columns from the dataframe that will be displayed in the Facility Supply Table
facilitySupplyTbl1<-subset(facilitySupply,select = c(listing_id, Supply.Score.Size, Supply.Score.Cost, address, listing_size, listing_rate_per_sqft_per_year, listing_rate_per_year, agent1_name, agent1_email_addresses, agent1_phone, vacant_land, lease_term))

## Defines drop down list options for school year
schYrDD <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")

## Defines tooltip css/html tab to customize width
popoverTempate <- 
  '<div class="popover popover-lg" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'


## Defines orange square marker (potential facilities) for find a facility map
icon.square <- makeIcon(
                   iconUrl = "https://www.pngfind.com/pngs/m/41-419073_orange-colour-box-orange-color-transparent-png-png.png",
                   iconWidth = 20,
                   iconHeight = 20
                       )


# Shiny UI (where all of the panels and tabs in the interface exist) ----------------------------------------------------------

ui <- fluidPage(
  
  ## Title panel created for rendering other panels in UI
  
  titlePanel(
  
    tags$head(
      tags$style(HTML("
                    @import url('//o180.org/facilities/style.css');
                    ")))),
  
  
  ## Panel for Finda a Region tab
  tabsetPanel(
    
    id="display_tab",
    
    tabPanel("Step 1: Find a Region for Your School", 
             
             fluidRow(
               br(),
               column(width =1),
               column(width = 4, 
                      ## Title and guiding text for school year dropdown
                      h3(strong("OPENING SCHOOL YEAR")),
                      h4("What school year do you plan to open your school?"),
                      ## School year dropdown 
                      selectInput("schoolYear",label = NULL, schYrDD, selected = "2022-23")),
               column(width =2),
               column(width =4,
                      ## Title and button for tool tip of grade level dropdown
                      h3(strong("GRADE LEVEL"),bsButton("I1", label = "", icon = icon("info-circle"), style = "default", size = "extra-small")),
                      ## Text for grade level dropdown tooltip button  
                      bsPopover(id = "I1",title = NULL, content = paste("If the school you plan to open will serve more than one grade level, review regional need for each grade level by toggling the drop down for each (e.g., if you plan to open a K-8, review regional need for ES and MS and identify zip codes where need overlaps)","<br>","<br>",
                                                                 "As there are fewer schools at higher grade levels (e.g., HS), there will be fewer color filled polygons when higher grade levels are selected")),
                      ## Guiding text for grade level dropdown
                      h4("Which school level do you plan to serve?"),
                      ## Grade level dropdown
                      selectInput("schoolLevel", label = NULL, list("ES","MS","HS"), selected = "ES"))),
            fluidRow(
               column(width = 1),
               column(width = 10,
                      ## Title for regional need map
                      h3("Regional Need Map"))),
            fluidRow(
              column(width = 1),
              column(width = 10,
                      ## Guiding text for regional need map
                      p("This map shows the relative need of zip codes. Darker areas indicate higher levels of need for high-quality seats. Level of need is calculated based on a region's average school quality, facilities utilization rate (enrollment divided by seat capacity), and surplus enrollment (enrollment beyond seat capacity). This is different from NSPCSA definition of need which includes factors of school quality & demographics."))),       
            fluidRow(
              column(width = 1),
              column(width = 10,
                     ## Call screenshot JS package to map
                     tags$head(
                       tags$script(src = jsfile),
                       tags$style(type="text/css",
                                  "#map{
                            height: calc(90vh - 100px) !important;
                            }")),
                     ## Regional need map output
                     leafletOutput("heatMap", height = 600),
                     )),
             br(),
             br(),
             br(),         
             fluidRow(
               ## css/html tag for custum width of regional need table tooltip 
               tags$head(
                 tags$style(HTML(".popover.popover-lg {width: 700px; max-width: 700px;}"))
               ),
             column(width = 1),
             column(width = 4, 
                    ## Regional need table title and tooltip button icon
                    h3("Regional Need Table", bsButton("I2", label = "", icon = icon("info-circle"), style = "default", size = "extra-small"))),
                    ## Includes regional need table tooltip text        
                    bsPopover(id = "I2", title = NULL,
                             content =  paste(strong("COLUMN DEFINITIONS"),"<br>",
                                    strong("Regional Need:"),"Categories from Low to Very high that indicate the need for high-quality school seats","<br>",
                                    strong("Regional Need Score:"),"The sum of the weighted School Quality, Utilization and Surplus Enrollment need factors (see the notes & instructions tab for more information)","<br>",
                                    strong("Zip Code:"),"The zip code data has been aggregated for","<br>",
                                    strong("Average Quality:"),"The average school quality scores of all schools within the zip code based on weighted enrollment","<br>",
                                    strong("Average Building Utilization:"),"The total enrollment for the zip code divided by the total building capacity of the zip code","<br>",
                                    strong("Total Projected Surplus Enrollment:"),"The number of students enrolled across schools that exceed the total building capacity of a zip code","<br>",
                                    strong("Total Portables"),"The total number of portable classrooms in use within a zip code","<br>",
                                    strong("% FRL:"),"The percentage of students within a zip code eligible to receive free or reduced priced lunch (FRL)","<br>",
                                    strong("% ELL:"),"The percentage of students within a zip code identified to be English Language Learners (ELL)","<br>",
                                    strong("% IEP:"),"The percentage of students within a zip code with Individualized Education Programs (IEP)",
                                    sep="<br>"), placement="bottom", options=list(template = popoverTempate))
                  ),
             fluidRow(    
                    column(width = 1),
                    column(width = 10, 
                           ## Regional need table guiding text
                           p("This table provides more information on the zip code level need scores reflected in the map. Higher need scores indicate a greater need for high-quality school seats. Zip code level school data has been summarized here by row and include the factors that make up the need score as well as demographic rates."),
                           downloadButton("downloadData1", "Download Regional Need Table as csv"))),
             
              
            fluidRow(
             column(width = 1),
             column(width = 10,
                    ## Regional need table output
                    DTOutput("ZipDemandTbl"))
             
             )
             ),
    
    ## Create find a facility tab --------
    tabPanel("Step 2: Find a Facility", 
             fluidRow(
               tags$head(
                 tags$style(type="text/css",
                            "h2{margin-bottom:30px;}")),
               column(width = 1),
               column(width = 3,
                      ## Title and guiding text for summary of selections made in find a region tab
                      h3(strong("FIND A REGION SELECTIONS")),
                      h4("These are selections from the Find a Region tab that inform the ouputs of this tab."),
                      h4(strong("Opening School Year "), textOutput("selectedSYText")),
                      h4(strong("Grade Level "), textOutput("selectedSLText")),
                      
                      ## Title and tooltip button icon for attendance zone (or zip) dropdown
                      h3(strong("ATTENDANCE ZONE (OR ZIP)", bsButton("I3", label = "", icon = icon("info-circle"), style = "default", size = "extra-small"))),
                      ## Attendance zone (or zip) dropdown tooltip text
                      bsPopover(id = "I3",title = NULL, content = "To provide a greater number of schools to consider along within the existing school community, High school attendance zones are used for a school level view of need."),
                      ## Guiding text for attendance zone (or zip) dropdown
                      h4("Which high needs attendance zone or zip code would you like to explore?"),
                      ## Attendance zone (or zip) dropdown
                      selectInput("schoolZone", label = NULL, unique(schDemandMap$Demand.Region[order(schDemandMap$Demand.Region)]), selected = "Western (89032, 89106, 89107, 89108)"),  
                      ## Title, guiding text, and input for priority school dropdown 
                      h3(strong("PRIORITY SCHOOL")),
                      h4("Which existing school would you like to open closest to in the attendance zone selected above?"),
                      selectInput("prioritySchool", label = NULL, unique(schDemandMap$School.Name[order(schDemandMap$School.Name)])),
                      ## Title, guiding text, and inputs for search factor dropdowns 
                      h3(strong("SEARCH FACTORS")),
                      h4("How important to your facilities search are the following factors?"),
                      selectInput("costFactor", "Lower cost", c("Not important", "Somewhat important", "Very important"), selected = "Somewhat important"),
                      selectInput("sizeFactor", "Larger size", c("Not important", "Somewhat important", "Very important"), selected = "Somewhat important"),
                      selectInput("distanceFactor", "Shorter distance from Priority School", c("Not important", "Somewhat important", "Very important"), selected = "Somewhat important")
               ),
               
             column(width = 7,
               ## Title and guiding text for facilities identification map
               h3("Facility Identification Map"),
               p("Find an available facility to locate in: The map shows ", em("potential facilities "), "as orange square markers with rankings that indicate which facilities best meet your needs (1 is the highest). " 
               , em("Selected schools "), "are also shown as yellow to blue circle markers based on their level of need (darker indicates higher need) and match the selections made in the Find a Region tab within the selected attendance zone.    
               To add or remove schools that do not match your selections, you can click the radio button for ", em("other schools "), "in the top right of the map to view them as grey circle markers. Click the markers in the map to see more information."),
               ## Title and out of map legend for facilities identification map markers
               p(strong("Map Markers")),
               column(width = 1, 
                      img(src='facilities.png', height = '20px', width = '20px')),
               column(width = 3,
                      p("Potential Facilities", style = "font-size: 15px;")),  
               column(width = 1, 
                      img(src='selectedSchools.png', height = '20px', width = '20px')),
               column(width = 3,
                      p("Selected Schools", style = "font-size: 15px;")),
               column(width = 1, 
                      img(src='otherSchools.png', height = '20px', width = '20px'), ),
               column(width = 3,
                      p("Other Schools", style = "font-size: 15px;")),
               
               ## css/html tag to adjust map size  
               tags$style(type = "text/css", "#map {height: calc(80vh - 100px) !important;}"),
               ## Facilities identification map output
               leafletOutput("map"),
               
               )),
           
             br(),
             fluidRow(
               column(width = 1),
               column(width = 3, 
                      ## Facilities table title
                      h3("Facilities Table"))),
             
             fluidRow(    
               column(width = 1),
               column(width = 10, 
                      ## Facilities table guiding text
                      p("This table provides additional context and actionable contact information on the facilities displayed in the map above."),
                      downloadButton("downloadData", "Download Facilities Table as csv"))),
             fluidRow(
               column(width = 1),
               column(width = 10,
                     ## Facilities table output 
                     DTOutput("facilitySupply")))
    ),
    
    ## Tab for adding new facilities -----------         
    tabPanel("Add New Facility",
             ## Html iframe tag for new facilities google form
             tags$iframe(id = "googleform",
                         src="https://docs.google.com/forms/d/e/1FAIpQLSd4JKamYHAPx6VKpsWBCYiEnf0EyDk_W9MSbd8r19K4Pnb29w/viewform?embedded=true",
                         width = 640,
                         height = 2171,
                         frameborder = 0,
                         marginheight = 0)
    ),
    
    tabPanel("Additional Resources",
             fluidRow(
               column(
                 ## Calls markdown file with formatted additional resources text/links 
                 width=12,includeMarkdown("Additional-Resources.md"))
             )
             
             
             
             ),
    
    tabPanel("Notes & Instructions",
             fluidRow(
               column(
                 ## Calls markdown file with formatted notes and instructions text/links 
                 width=12,includeMarkdown("Clark-County-Notes-and-Instructions.md"))
             )
    )
    
  ))

#-----------------------------------------------------

## Server where interactions run and tables and maps are created

server <- function(input, output, session) {
  
  
  
  ## Create display table with demand information
  output$schDemandTbl <- renderDT({
    schDemandfin <- selectedDf() %>% 
      datatable(
        options = list(
                       
                      paging = FALSE, 
                       searching = FALSE, 
                       sort = FALSE, 
                       scrollY = "120px")) %>%
      
      ## Subset demand selected table
      schDemandfin <- subset(schDemandfin,select = c(Demand.Score, Demand.Region, Avg.School.Quality, Utilization, Surplus.Enrollment, Portables))
      ## Convert utilization to a percent
      schDemandfin <- schDemandfin %>%
        mutate(Utilization = percent(Utilization))
      ## Convert demand score to a percent
      schDemandfin <- schDemandfin %>%
        mutate(Demand.Score = percent(Demand.Score))
  })
  
  ## Creates and formats display table with zip code level demand information
  output$ZipDemandTbl <-renderDT({
    
    ZipDemandTbl <- selectedZipDemand() %>%
      datatable(
        colnames = c("Regional Need", "Regional Need Score", "Zip Code", "Average Quality", "Average Building Utilization", "Total Projected Surplus Enrollment", "Total Portables", "% FRL", "% IEP", "% ELL"), 
        rownames = FALSE,
        options = list(
          
          columnDefs = list(list(className = 'dt-right', targets = c(1,2,4,7,8,9))),
          paging = FALSE, 
          searching = FALSE,
          scrollY = "600px",
          scrollX = T)) 
      
    
  })
  
  ## Create reactive variable for attendance zone, school year, & school level selections
  selectedSZ <- reactive({
    input$schoolZone})
  
  selectedSY <- reactive({
    input$schoolYear})
  
  selectedSL <- reactive({
    input$schoolLevel})
  
  ## Update table based on user input
  selectedZipDemand <- reactive({
    ZipDemand.calculation(ZipCodeDemand, selectedSL(), input$schoolYear, DemographicYear, uFctr, qFctr, eFctr)
    })
  
  ## Update table based on user input
  selectedDf <- reactive({
    
    finalSchDemand[finalSchDemand$Year == selectedSY() & finalSchDemand$School.Level == input$schoolLevel,] 
    })
  
  
  
  ## Facilities identification map data table for subset set of selected schools based on user inputs
  selectedMDf <- reactive({
    schDemandMap[schDemandMap$Demand.Region == selectedSZ() & schDemandMap$Year == selectedSY() & schDemandMap$School.Level == selectedSL(),]
    }) 
  
  ## Facilities identification data table for subset set of other schools (schools not selected) based on user inputs
  nonselectedMDf <- reactive({
    schDemandMap[schDemandMap$Year == selectedSY() & schDemandMap$School.Level != selectedSL(),]
    })
  

  ## Create dataframe based on priority school selection
  selectedSchool <- reactive({
    schDemandMap[schDemandMap$School.Name == input$prioritySchool, ]
  })
  
  ## Facilities identification table updates based on user inputs
  selectedDf <- reactive({
    
    finalSchDemand[finalSchDemand$Year == input$schoolYear & finalSchDemand$School.Level == input$schoolLevel,] 
  })
  
  ## Generates output text for find a facility tab based on selections made in find a region tab 
  output$selectedSYText <- renderText({
    input$schoolYear
  })
  
  output$selectedSLText <- renderText({
    input$schoolLevel
  })
  
  ## Establishes reactive scaled value for supply score factors  
  selectedSize <- reactive({
    ifelse(input$sizeFactor == "Very important", 2, ifelse(input$sizeFactor == "Somewhat important", 1, 0))
    })
  selectedCost <- reactive({
    ifelse(input$costFactor == "Very important", 2, ifelse(input$costFactor == "Somewhat important", 1, 0))
  })
  selectedDist <- reactive({
    ifelse(input$distanceFactor == "Very important", 2, ifelse(input$distanceFactor == "Somewhat important", 1, 0))
  })

  
  selectedFDf <- reactive({
    
    
    ## Creates a vector of distances between selected school and facility
    facDist <- round(conv_unit(distm(facilitySupply[ , c("Longitude", "Latitude")], c(selectedSchool()$Longitude[1], selectedSchool()$Latitude[1]),
                                     fun = distHaversine), "m", "mile"),digits = 2)
    
  
    
    ## Create vector of addresses and then bind to distance vector
    listing_id <- facilitySupply$listing_id
    facSup <- data.frame(listing_id, facDist)
    
    ## Rename column
    colnames(facSup)[2] <- "Priority.School.Distance"
    
    distMax <- max(facSup$Priority.School.Distance, na.rm = TRUE)
    
    facSup <- facSup%>%
      mutate(Supply.Score.Distance = (1-(Priority.School.Distance/distMax)))
    
    ## Merge distances into facility supply table
    facilitySupply <- merge(facilitySupply, facSup, by = "listing_id")
    
    facilitySupply$listing_rate_per_year[is.na(facilitySupply$listing_rate_per_year)]<-0
    facilitySupply$Supply.Score.Size[is.na(facilitySupply$Supply.Score.Size)] <- 0
    facilitySupply$Supply.Score.Distance[is.na(facilitySupply$Supply.Score.Distance)] <- 0
    
    ##Scale facility preference factors
    discountFactor <- max(1, selectedSize() + selectedCost() + selectedDist())
    
    facilitySupply <- facilitySupply%>%
      mutate(Supply.Score = (Supply.Score.Size*(selectedSize() / discountFactor)) + (Supply.Score.Cost*(selectedCost()/ discountFactor)) + (Supply.Score.Distance*(selectedDist() / discountFactor)))
    
    ## Remove duplicates
    facilitySupply<-facilitySupply%>%distinct(listing_id, .keep_all=TRUE )
    
    ##Create a supply rank
    facilitySupply <- facilitySupply%>%
      mutate(Supply.Rank = rank(desc(facilitySupply$Supply.Score), na.last = TRUE, ties.method = "min"))
    
  })
  
  
  ## Create display table with supply information
  output$facilitySupply <- renderDT({
    ## Reorder columns
    facilitiestbl1 <- selectedFDf()
    
    
    facilitiestbl1 <- facilitiestbl1%>% 
      arrange(Supply.Rank)
    
    facilitiestbl1<-facilitiestbl1[c("Supply.Rank","Priority.School.Distance", "address", "listing_size", 
                                     "listing_rate_per_sqft_per_year", "listing_rate_per_year", "agent1_name", 
                                     "agent1_email_addresses", "agent1_phone", "vacant_land", "lease_term")]
    
    ## Formats numeric facilities costs into currency format
    facilitiestbl1$listing_rate_per_sqft_per_year <- dollar_format()(facilitiestbl1$listing_rate_per_sqft_per_year)
    facilitiestbl1$listing_rate_per_year <- dollar_format()(facilitiestbl1$listing_rate_per_year)
    
    ## Rename columns
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="address"] <- "Address"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="listing_size"] <- "Size (SqFt)"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="listing_rate_per_sqft_per_year"] <- "Cost per SqFt"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="listing_rate_per_year"] <- "Annual Cost"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="agent1_name"] <- "Listing Agent"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="agent1_email_addresses"] <- "Agent Email"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="agent1_phone"] <- "Agent Phone"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="vacant_land"] <- "Vacant Land"
    colnames(facilitiestbl1)[colnames(facilitiestbl1)=="lease_term"] <- "Lease Term"
    
    ## Refines formatting of facilities table
    facilitiestbl1 <- facilitiestbl1%>%
      arrange((Supply.Rank)) %>% 
      datatable(
        colnames = c("Priority Facility Rank", "School Distance (mi)", "Address", "Size (SqFt)", "Cost per SqFt", "Annual Cost", "Listing Agent", "Agent Email", "Agent Phone#", "Vacant Land", "Lease Term"), 
        rownames = FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width='400px', targets = 2), list(className='dt-right', targets = c(4,5))),
          paging = FALSE, 
          searching = FALSE,
          scrollY = "600px",
          scrollX = T)) 
    
  })
  
  ## Initial find a region map rendering to include screenshot feature
  observe({
    req(input$display_tab == "Step 1: Find a Region for Your School")
    
    output$heatMap <- renderLeaflet({
      
      leaflet() %>% 
        
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        ## Add screenshot function
        onRender(
          "function(el, x) {
                            L.easyPrint({
                              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'], 
                              filename: 'Map',
                              exportOnly: true,
                              hideControlContainer: false
                            }).addTo(this);
                    }"
        )
      
                            
    })
    
  })
  
  ## Second find a region map rendering to generate color coded polygons
  observe({
    
    ## Merge and remerge changes to polygon data based on user selections to adjust the color coded polygons
    HeatPolygons <- sp::merge(ZipLines, selectedZipDemand(), by.x = 'ZCTA5CE10', by.y = "Zip_Code")
    
    ## Defines color palette for polygons based on defined categories of need
    pal <- colorFactor(palette = c("#d5f7da","#98eddd","#6ec4e6","#318bcc"), levels = c("Low","Moderate","High","Very High"))
    
    req(input$display_tab == "Step 1: Find a Region for Your School")
    
    leafletProxy("heatMap") %>% 
      ## Clears map of previous polygons when selections change 
      clearControls() %>% 
      clearGroup("heatPoly") %>% 
      setView (lng = -115.156496, lat = 36.174574, zoom = 10.4)  %>% 
      ## Adds color coded polygons with tooltips
      addPolygons(data = HeatPolygons, 
                  group = "heatPoly",
                  color = "grey",
                  ## Define polygon colors using color palette defined above
                  fillColor = ~pal(HeatPolygons$Regional.Need),
                  fillOpacity = 0.7,
                  weight = 3, 
                  ## Polygon tooltip text and variables
                  popup = ~paste("<strong>","Zip Code:","</strong>", ZCTA5CE10,"<br>", 
                                 "<strong>","Regional Need:","</strong>", HeatPolygons$Regional.Need,"<br>",
                                 "<strong>","Regional Need Score:","</strong>", HeatPolygons$Demand.Score,"<br>",
                                 "<strong>","Regional Quality:","</strong>",round(HeatPolygons$weighted_quality, digits = 1),"<br>",
                                 "<strong>","Average Building Utilization:","</strong>",HeatPolygons$weighted_utilization, "<br>",
                                 "<strong>","Total Surplus Enrollment:","</strong>", HeatPolygons$total.surplus.enrollment,"<br>",
                                 "<strong>","Total Portables:","</strong>", HeatPolygons$total.portables,"<br>",
                                 "<strong>","% FRL:","</strong>", HeatPolygons$FRL,"<br>",
                                 "<strong>","% IEP:","</strong>", HeatPolygons$IEP,"<br>",
                                 "<strong>","% ELL:","</strong>", HeatPolygons$ELL,"<br>","<br>"
                                 )) %>% 
      ## Adds legend that corresponds to colors and categories defined above
      addLegend("topright", 
                colors = c("#d5f7da","#98eddd","#6ec4e6","#318bcc"),
                labels = c("Low", "Moderate", "High", "Very High"),
                title = paste("Regional Need"),
                opacity = 1)
  })
  
  ## Track and update find a facility map data frame based on the selected priority school
  observe({
    updateSelectInput(session, "prioritySchool", choices = selectedMDf()$School.Name)
  })
  
  ## Create map output
  output$map<-renderLeaflet({
    
    ## Create map and set defaut zoom
    m<-leaflet(data=facilitySupply, options = leafletOptions(minZoom = 6))%>%
      
      ## Specify background layout
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    
  })
  
  observe({
    
    req(input$display_tab == "Step 2: Find a Facility")
    ## Define dataframe for potential facilities markers 
    mapFac <- selectedFDf()
    mapFac <- mapFac%>%
      arrange(Supply.Rank)
    mapFac<-distinct(mapFac, Longitude, Latitude, .keep_all=TRUE )
    
    leafletProxy("map",data = mapFac)%>%
      ## Set bounds of map to the max and min lat lon of avaiable facilities markers
      setMaxBounds(~min(Longitude,na.rm=TRUE)-10, 
                   ~min(Latitude,na.rm=TRUE)-10, 
                   ~max(Longitude,na.rm=TRUE)+10, 
                   ~max(Latitude,na.rm=TRUE)+10)%>%
      ## Adds potential facilities markers to the find a facility map
      addMarkers(
        ~Longitude,
        ~Latitude,
        ## Renders orange square shape defined in server above 
        icon = icon.square,
        ## Includes potential facilities tooltip text and variables 
        popup = ~paste("<strong>","Address:","</strong>", address,"<br>", 
                       "<strong>","Zip Code:","</strong>", Zip_Code,"<br>",
                       "<strong>","Annual Cost:","</strong>", "$",listing_rate_per_year,"<br>",
                       "<strong>","Size (SqFt):","</strong>", listing_size,"<br>",
                       "<strong>","Cost per SqFt:","</strong>", "$",listing_rate_per_sqft_per_year,"<br>" 
        )) 
        
    
  })
  
  
  ## Create rank labels for users custom top ten potential facilities options
  observe({
    ## Create new data frame with just top ten facilities for labeling
    rankLabels <- selectedFDf()
    rankLabels <- rankLabels%>%
      arrange(Supply.Rank)
    rankLabels <- head(rankLabels,10)
    rankLabels<-distinct(rankLabels, Longitude, Latitude, .keep_all=TRUE )
    
    req(input$display_tab == "Step 2: Find a Facility")
    
    leafletProxy("map",data = rankLabels)%>%
      clearGroup('labels')%>% 
      
      ## Adds rank labels above potential facilities markers
      addLabelOnlyMarkers(
        group = "labels",
        ~Longitude,
        ~Latitude,
        label =  ~as.character(Supply.Rank),
        labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T)
      )
  })
  
  ## Creates find a facility map overlay options
  observe({
    
    req(input$display_tab == "Step 2: Find a Facility")
    
    leafletProxy("map") %>% 
      
      ## Adds zip code line overlay to map when radio button is selected
      addPolylines(data = ZipLines,
                   group = "Zip Code",
                   color = "Blue",
                   weight = 1.5,
                   fillOpacity = 0) %>%
      ## Adds zip code labels to map when radio button is selected
      addLabelOnlyMarkers(data = ZipCenterPoints, ~lng, ~lat, 
                          group = "Zip Code",
                          label =  ~as.character(zip), 
                          labelOptions = labelOptions(noHide = T,
                                                      direction = 'center', 
                                                      textOnly = T,
                                                      textsize = "20px",
                                                      style = list(
                                                        "color" = "Blue"
                                                      ))) %>% 
      ## Adds opportunity zone polygon overlay to map when radio button is selected
      addPolygons(data = OppZones,
                  group = "Opportunity Zones",
                  color = "Blue",
                  weight = 2,
                  fillColor = "Blue",
                  fillOpacity = 0.05) %>%
      
      ## Adds other schools markers to map when radio button is selected
      addCircleMarkers(data = nonselectedMDf(),
                       group = "Other Schools",
                       ~Longitude,
                       ~Latitude,
                       
                       radius = 7,
                       color= "#C0C0C0",
                       stroke=TRUE,
                       opacity=0.5,
                       weight=2,
                       fillOpacity=0.5,
                       ## Includes other school tooltip text and variables
                       popup = ~paste("<strong>","School Name:","</strong>", School.Name,"<br>",
                                      "<strong>","Zip Code:","</strong>", Zip_Code,"<br>",
                                      "<strong>","School Quality:","</strong>", SchoolQuality_Avg,"<br>",
                                      "<strong>","Utilization:","</strong>", percent(Utilization, accuracy = 0.1),"<br>",
                                      "<strong>","Surplus Enrollment:","</strong>", Surplus.Enrollment,"<br>",
                                      "<strong>","Senate District:","</strong>", Senate, "<br>",
                                      "<strong>","Assembly District:","</strong>", Assembly
                       )) %>% 
      ## Defines menue for overlay options
      addLayersControl(overlayGroups = c("Zip Code","Opportunity Zones", "Other Schools"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      ## Hides (or clicks off) zip code and opportunity zones overlays as default until selected
      hideGroup(c("Zip Code","Opportunity Zones"))
      

  })
  
  observe({
    
    req(input$display_tab == "Step 2: Find a Facility")
    
    
    # Sets blue color palette shaded by school demand score
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = schDemandMap$Demand.Score)
    
    leafletProxy("map",data = selectedMDf())%>%
      ## Set bounds of map to the max and min lat lon of schools plus a marginal buffer to view more of the map
      fitBounds(
        ~min(Longitude,na.rm=TRUE)-0.025, 
        ~min(Latitude,na.rm=TRUE)-0.025, 
        ~max(Longitude,na.rm=TRUE)+0.025, 
        ~max(Latitude,na.rm=TRUE)+0.025
      )%>%
      
      clearGroup('schoolPoints') %>%
      clearControls()%>%
      
      ## Add markers to map for each school
      addCircleMarkers(
        group = "schoolPoints",
        ~Longitude,
        ~Latitude,
        
        
        ## Sets fill and stroke color(the same)
        color=~pal(Demand.Score),
        stroke=TRUE,
        
        ## Sets opacity and weight
        opacity=0,
        weight=2,
        fillOpacity=0.8,
        
        ## Text that will appear when circle is clicked
        popup = ~paste("<strong>","School Name:","</strong>", School.Name,"<br>", 
                       "<strong>","School Need Score:","</strong>", percent(Demand.Score, accuracy = 0.1),"<br>",
                       "<strong>","Zip Code:","</strong>", Zip_Code,"<br>",
                       "<strong>","Senate District:","</strong>",Senate, "<br>",
                       "<strong>","Assembly District:","</strong>", Assembly
                      ))%>%  
  
      
      ## Adds a legend for the color scale
      addLegend("bottomright", pal = pal, 
                values =~ Demand.Score,
                labFormat = labelFormat(suffix = "%", transform = function(x) 100* x),
                title = "School Need Score",
                opacity = 1
      ) 
    
    })

 ## Renders need table .csv download when download button is pushed
 output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Need Table ",input$schoolLevel,"_",input$schoolYear,"_", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(selectedZipDemand(), file, row.names = FALSE)
    }
  )
 ## Facilities table .csv download when download button is pushed
 output$downloadData <- downloadHandler(
    filename = function() {
      paste("Facilities Table_",Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(selectedFDf(), file, row.names = FALSE)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)
