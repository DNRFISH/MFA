# ============================================================
# MFA Package Development Workflow
# Run from the MFA package root directory
# ============================================================

#when working on code, use this to re-load dev package
devtools::load_all()

# ------------------------------------------------------------
# 1. UPDATE DOCUMENTATION
# ------------------------------------------------------------

# Edit documentation as needed:
# - R/ function files: roxygen documentation
# - README.Rmd: README-specific content
# - vignettes/sections/MFA-tutorial.Rmd: shared tutorial (in the future this can be split)
# - vignettes/MFA-introduction.Rmd: vignette introduction

# Generate function documentation and NAMESPACE
devtools::document()

# Generate README.md from README.Rmd
# Includes content from the shared tutorial
devtools::build_readme()


# ------------------------------------------------------------
# 2. RUN TESTS
# ------------------------------------------------------------

# Run testthat tests independently
devtools::test()


# ------------------------------------------------------------
# 3. CHECK PACKAGE
# ------------------------------------------------------------

# Check package structure, documentation, dependencies,
# and vignette building.
#
# Tests are excluded here because they previously caused
# problems during R CMD check (October 2026).
# Tests are run separately above.

devtools::check(args = "--no-tests")

# Once test issues are resolved, use the full check instead:
# devtools::check()


# ------------------------------------------------------------
# 4. BUILD RELEASE
# ------------------------------------------------------------

# Before a release:
# - Update Version in DESCRIPTION
# - Commit changes and push to GitHub
# - Confirm documentation and tests are satisfactory

# Build Windows binary, including vignette
# Uncomment when preparing a release

# pkgbuild::build(
#   binary = TRUE,
#   vignettes = TRUE
# )


# ------------------------------------------------------------
# 5. VERIFY INSTALLED PACKAGE
# ------------------------------------------------------------

# After installing the release ZIP, verify:

# vignette("MFA-introduction", package = "MFA")
# MFA::run_MFA_app()


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
