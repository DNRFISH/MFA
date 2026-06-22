library(DBI)
library(dplyr)
library(purrr)

# #PRODUCTION ENVIRONMENT
# con <- dbConnect(odbc(),
#                  Driver = "ODBC Driver 17 for SQL Server",
#                  Server = "DNRSQLWEB",
#                  Database = "FISHHUB",
#                  Trusted_Connection = "yes")

#Reporting database
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "DNRSQLWEB",
                 Database = "FISHReport",
                 Trusted_Connection = "yes")

# get all table names
tables <- dbListTables(con)

# #subset to actual data; NOTE:if a new table is created after vWaterBody, this will break -- resolved with reporting databas
# datTables <- tables[1:which(tables=="vWaterBody")]

# build table -> fields mapping
field_list <- map_dfr(datTables, function(tbl) {
  tryCatch(
    tibble(
      table_name = tbl,
      field_name = dbListFields(con, tbl)
    ),
    error = function(e) {
      NULL
    }
  )
})

#test one out
dat <- dbReadTable(con, "SurveyEffort")

#save it
write.csv(field_list,"FISH_reporting_table_structure_2026.06.05.csv",row.names = F)
