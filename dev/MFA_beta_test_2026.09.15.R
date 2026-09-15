#install packages
install.packages("remotes")
remotes::install_github("DNRFISH/MFA") #note- I think you need to be on the repo in order to run this

#load package
library(MFA)

#check package documentation
help(package = "MFA")

#connect to FISHub
con <- FISHub_connect()

#look at help file for query
?FISH_query

#run a query
FISH_Data <- FISH_query(con,QueryType = "Catch",SurveyId = 805)