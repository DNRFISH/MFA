remotes::install_github("DNRFISH/MFA", ref = "mapping")
library(MFA)

con<-FISHub_connect()

lakeSNT<-FISH_query(con,QueryType = "Catch",Year = 2025,SurveyCategory = "Status & Trends",WaterTypeAbbr = "IL")

MFA_map(lakeSNT)
