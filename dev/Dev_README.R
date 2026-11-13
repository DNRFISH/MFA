# ============================================================
# MFA DEVELOPER README
# ============================================================
#
# DEVELOPMENT WORKFLOW
#
# Follow these steps when adding or modifying MFA functions.
#
# 1. MODIFY FUNCTION
#    - Create or update the function in R/.
#    - Follow existing MFA naming and coding conventions.
#    - Update roxygen documentation (parameters, return
#      values, examples, imports, etc.).
#    - Consider impacts on other functions that use the
#      modified function.
#    - If applicable, update the Shiny app to incorporate
#      new or modified functionality.
#
# 2. RELOAD AND TEST INTERACTIVELY
#    - Reload the development version of MFA.
#    - Run the function with representative FISHub data.
#    - Check expected outputs, missing values, warnings,
#      and edge cases.
#    - Check multiple-survey inputs where applicable.
#
# 3. ADD OR UPDATE AUTOMATED TESTS
#    - Create or modify tests in tests/testthat/.
#    - Test expected results and important edge cases.
#    - Add regression tests for corrected bugs.
#    - Run the relevant test file, then all tests.
#
# 4. UPDATE TUTORIAL AND EXAMPLES
#    - Update vignettes/sections/MFA-tutorial.Rmd when
#      adding or changing user-facing functionality.
#    - Include examples showing typical usage.
#    - Update vignettes/MFA-introduction.Rmd if needed.
#    - Update README.Rmd if the change affects the
#      package overview or installation instructions.
#    - Internal-only functions generally do not need
#      tutorial updates.
#
# 5. UPDATE DOCUMENTATION
#    - Run devtools::document().
#    - Review generated help files and NAMESPACE changes.
#    - Rebuild README.md if README.Rmd or shared tutorial
#      content has changed.
#
# 6. VALIDATE CHANGES
#    - Run all automated tests.
#    - Run the package check.
#    - Review and resolve new warnings, errors, or notes.
#    - Verify Shiny app functionality if affected.
#
# 7. COMMIT CHANGES
#    - Review modified files.
#    - Commit code, tests, and documentation together.
#    - Use a descriptive commit message.
#    - Push changes to GitHub.
#
# 8. PREPARE RELEASE (WHEN APPLICABLE)
#    - Update version in DESCRIPTION.
#    - Update NEWS.md with user-facing changes.
#    - Complete documentation, tests, and package checks.
#    - Build source package and Windows binary.
#    - Install and verify the release package.
#
# ============================================================
# COMMON DEVELOPMENT COMMANDS
# ============================================================

# Reload development package after code changes
devtools::load_all()

# Generate documentation and update NAMESPACE
devtools::document()

# Run a specific test file (e.g., test-catchSummary.R)
devtools::test(filter = "app-live")
devtools::test(filter = "effortSummary")

# Run all tests
devtools::test()

# Build README.md
devtools::build_readme()

# Check package (tests currently run separately)
devtools::check(args = "--no-tests")


# ------------------------------------------------------------
# BUILD RELEASE
# ------------------------------------------------------------

# Before a release:
# - Update Version in DESCRIPTION
# - Commit changes and push to GitHub
# - Confirm documentation and tests are satisfactory

# Build Windows binary, including vignette

# Step 1: Build source package, including vignettes
source_pkg <- pkgbuild::build(
  binary = FALSE,
  vignettes = TRUE
)

# Step 2: Build Windows binary from source package
zip_file <- pkgbuild::build(
  path = source_pkg,
  binary = TRUE
)

# Step 3: Verify vignette is included
zip_contents <- unzip(zip_file, list = TRUE)

stopifnot(
  "MFA/doc/MFA-introduction.html" %in% zip_contents$Name,
  "MFA/Meta/vignette.rds" %in% zip_contents$Name
)

message("MFA release built successfully: ", zip_file)


# ------------------------------------------------------------
# VERIFY INSTALLED PACKAGE
# ------------------------------------------------------------

# After installing the release ZIP, verify:

# vignette("MFA-introduction", package = "MFA")
# MFA::run_MFA_app()






#for adding new data tables
#save raw data in data-raw
#convert to .rda using the following example code
# Status_and_Trends_extra_efforts <- readr::read_csv("data-raw/Status_and_Trends_extra_efforts.csv")
# usethis::use_data(Status_and_Trends_extra_efforts, overwrite = TRUE)
#document data with .R file; put all the relevant data source and edit details in there
