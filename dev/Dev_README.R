#developer Read Me

#general work flow for updating package

#Be sure to do a pull to make sure you have the most recent version of files
#**Update this in the future to describe branching**
#Load the package
devtools::load_all()

##After making edits
#update documenation
devtools::document()
#run a package test
devtools::check()
# #run the package tests - note this is included in check()
# devtools::test()

#build package


#for adding new functions
#create file and save in MFA/R
#add documentation and examples
#develop test file using usethis::use_test("function_name")
#add to README

#for adding new data tables
#save raw data in data-raw
#convert to .rda using the following example code
# Status_and_Trends_extra_efforts <- readr::read_csv("data-raw/Status_and_Trends_extra_efforts.csv")
# usethis::use_data(Status_and_Trends_extra_efforts, overwrite = TRUE)
#document data with .R file; put all the relevant data source and edit details in there
