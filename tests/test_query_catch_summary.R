#testing MFA query and catch summaries

###############################################################################################################################
##Install required packages
###############################################################################################################################
#load dplyr and gpplot-- used often enough that this is easier than specifying them in code
library(dplyr)
library(odbc)
library(DBI)
library(stringr)
library(lubridate)
library(ggplot2)


###############################################################################################################################
##Connect to databases
###############################################################################################################################
#Reporting database
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "DNRSQLWEB",
                 Database = "FISHReport",
                 Trusted_Connection = "yes")

###############################################################################################################################
##load the functions that are ready
###############################################################################################################################
invisible(lapply(list.files("./code/", full.names = TRUE,recursive = T), source))


###############################################################################################################################
##Read in a single table (for testing)
###############################################################################################################################
SurveyGear <- tbl(con, "SurveyEffortDetail")%>%
  left_join(tbl(con, "Gear")%>%select(GearId,GearType),by = "GearId")%>%
  collect()
SurveyGearNAs<-SurveyGear%>%filter(is.na(EffortTotalQuantity))
table(SurveyGearNAs$GearType)

GearEffortMeasurement <- tbl(con, "GearEffortMeasurement")%>%
  #left_join(tbl(con, "Gear")%>%select(GearId,GearType),by = "GearId")%>%
  collect()

###############################################################################################################################
##Query data
###############################################################################################################################
#Read in Data
#single survey
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 9320)
#single survey with an effort with zero captures
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 13601)
#all surveys from a waterbody
FISH_Data <- FISH_query(con,QueryType = "Survey",WaterBodyName = "Lake Orion")
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 1162)

#all SnT surveys from 2025
FISH_Data <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Status & Trends",Year=2025)

###############################################################################################################################
##Catch Summaries
###############################################################################################################################

#examples to test out catch/inch tables
catchSum_inch <- catch_summary_table(FISH_query(con,QueryType = "Efforts",SurveyId = 1228))
catchSum_species <- catch_summary_table(FISH_query(con,QueryType = "Efforts",SurveyId = 16923))

#compare to survey outputs (from Cleyo)
#Lake Orion, 2023
#total count = 1005 LMB for both
#age count = 361 for both (can use this later for age data test)
catchSum <- catch_summary_table(FISH_query(con,QueryType = "Efforts",SurveyId = 1162))
scaleEnvelopeTest <- tbl(con, "ModuleDataScaleEnvelope") %>%
  select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
  left_join(tbl(con, "SpeciesStrain") %>%
              select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
  filter(SurveyId==1162)%>%
  collect()

#Lake Sixteen, 2024
#catch summary totals and length ranges are OK, but avg lengths are off
#confirm totals: 5 NOP in catch summary, 4 in age data- was there 5 or 9 total caught?
catchSum <- catch_summary_table(FISH_query(con,QueryType = "Efforts",SurveyId = 805)) 
scaleEnvelopeTest <- tbl(con, "ModuleDataScaleEnvelope") %>%
  select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
  left_join(tbl(con, "SpeciesStrain") %>%
              select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
  filter(SurveyId==805)%>%
  collect()

###############################################################################################################################
##Age-length Summaries
###############################################################################################################################
#both 805 and 1162 of these look good!
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
ageDat<-age_length_summary(FISH_Data,OutputType = "RawData")
ageDat<-age_length_summary(FISH_Data,OutputType = "Table")
age_length_summary(FISH_Data,OutputType = "Figure")

FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 1162)
ageDat<-age_length_summary(FISH_Data,OutputType = "Table")
age_length_summary(FISH_Data,OutputType = "Figure")


###############################################################################################################################
##CPUE Summaries
###############################################################################################################################
#
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
