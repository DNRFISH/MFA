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


#for adding new functions
#create file and save in MFA/R
#add documentation and examples
#develop test file using usethis::use_test("function_name")
#add to README