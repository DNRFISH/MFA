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
#convert to .rda using the following code
# flagged_surveys_species_2026.09.02 <- readr::read_csv("data-raw/flagged_surveys_species_2026.09.02.csv")
# usethis::use_data(flagged_surveys_species_2026.09.02, overwrite = TRUE)
