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
Effort<-tbl(con, "SurveyEffort") %>%
  select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId)%>%
  left_join(tbl(con, "SurveyEffortDetail"),by="SurveyEffortId")%>%
  left_join(tbl(con, "Gear") %>%select(GearId, GearType),by="GearId")%>%
  select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId, BeginningEffortTimestamp, EndingEffortTimestamp, GearType,EffortNumberofGearUsed, EffortTotalQuantity, 
         EffortTotalMeasurement, EffortAlternateQuantity, EffortAlternateMeasurement)%>%
  filter(SurveyId==13601)%>%
  collect()

###############################################################################################################################
##Query data
###############################################################################################################################
#Read in Data
#single survey
FISH_Data <- FISH_query(con,QueryType = "Survey",SurveyId = 805)
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
FISH_Data <- FISH_query(con,QueryType = "Catch",SurveyId = 805)

#all surveys from a waterbody
FISH_Data <- FISH_query(con,QueryType = "Survey",WaterBodyName = "Lake Orion")
FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 16923)

#all SnT surveys from 2025
FISH_Data <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Status & Trends",Year=2025)

#all surveys that caught bowfin in 2025
FISH_Data <- FISH_query(con,QueryType = "Survey",Species="Bowfin",Year=2025)
FISH_Data <- FISH_query(con,QueryType = "Efforts",Species="Bowfin",Year=2025)
FISH_Data <- FISH_query(con,QueryType = "Catch",Species="Bowfin",Year=2025)


###############################################################################################################################
##Catch Summaries
###############################################################################################################################
#compare to survey outputs (from Cleyo)
#Lake Orion, 2023
#total count = 1005 LMB for both
#age count = 361 for both (can use this later for age data test)
catchSum <- catch_summary_table(FISH_query(con,QueryType = "Catch",SurveyId = 1162))
scaleEnvelopeTest <- tbl(con, "ModuleDataScaleEnvelope") %>%
  select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
  left_join(tbl(con, "SpeciesStrain") %>%
              select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
  filter(SurveyId==1162)%>%
  collect()

#Lake Sixteen, 2024 - see issue #4 
#catch summary totals and length ranges are OK, but avg lengths are off; issue in FCS/FISH
#confirm totals: 5 NOP in catch summary, 4 in age data- was there 5 or 9 total caught? - confirmed
catchSum <- catch_summary_table(FISH_query(con,QueryType = "Catch",SurveyId = 805))
catchSum <- catch_summary_table(FISH_query(con,QueryType = "Catch",GearType = "LMFYKE",SurveyId = 805))

scaleEnvelopeTest <- tbl(con, "ModuleDataScaleEnvelope") %>%
  select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
  left_join(tbl(con, "SpeciesStrain") %>%
              select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
  filter(SurveyId==805)%>%
  collect()

###############################################################################################################################
##Detections by Year
###############################################################################################################################
catchData <- FISH_query(con,QueryType = "Catch",WaterBodyName = "Lake Orion")
detectionByYearSum<-detectionByYear(catchData)

###############################################################################################################################
##Length frequency and RSD-- have gone back and forth on whether this should be a standalone function or repeated code
###############################################################################################################################
lengthFreqData <- lengthFreqByEffort(SurveyEffortData=FISH_query(con,QueryType ="Efforts",SurveyId = 805),OutputType="Table")
RSDsum<-RSD(lengthFreqData)

###############################################################################################################################
##Age-length Summaries  need to update these after the query functions are updated
###############################################################################################################################
#both 805 and 1162 of these look good!
SurveyEffortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
ageDat<-age_length_summary(SurveyEffortData,OutputType = "RawData")
ageDat<-age_length_summary(SurveyEffortData,OutputType = "Table")
age_length_summary(SurveyEffortData,OutputType = "Figure")

SurveyEffortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 1162)
ageDat<-age_length_summary(SurveyEffortData,OutputType = "Table")
age_length_summary(SurveyEffortData,OutputType = "Figure")


###############################################################################################################################
##CPUE Summaries
###############################################################################################################################


