# Michigan Fisheries Analysis (MFA)

Michigan Fisheries Analysis (MFA) is an R package for querying and summarizing fisheries data from the Michigan Department of Natural Resources FISHub database.

The package provides functions for retrieving survey, effort, and catch data and for producing commonly used fisheries summaries, including catch summaries, catch per unit effort (CPUE), length-frequency distributions, age-length summaries, detection summaries, and relative stock density (RSD).

Future releases will include expanded functions for other FISHub modules (e.g., stocking), other fisheries data (e.g., Master Angler) and external data.

## Installation

MFA is currently under development and is not yet available on CRAN.

The development version can be installed from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("YOUR-REPOSITORY/MFA")
```

## Connecting to FISHub

MFA provides a connection function that uses Windows integrated authentication to connect to the FISHub database.

``` r
library(MFA)

con <- FISHub_connect()
```

The connection can then be used with MFA functions that query the database.

When finished, close the database connection:

``` r
DBI::dbDisconnect(con)
```

## Querying FISHub

`FISH_query()` retrieves survey, effort, or catch data from FISHub.

For example, to retrieve Status & Trends surveys from 2025:

``` r
surveys <- FISH_query(
  con,
  QueryType = "Survey",
  SurveyPurpose = "Status & Trends",
  Year = 2025
)
```

To retrieve effort data for a specific survey:

``` r
efforts <- FISH_query(
  con,
  QueryType = "Efforts",
  SurveyId = 805
)
```

Catch data can also be filtered by survey, species, gear, waterbody, or other available criteria:

``` r
catch <- FISH_query(
  con,
  QueryType = "Catch",
  SurveyId = 805,
  GearType = "SMFYKE"
)
```

## Fisheries summaries

MFA includes functions for summarizing fisheries data.

### Catch summary

`catchSummary()` summarizes to produce one row for each survey and species combination.The summary includes total catch, weighted average length, minimum and maximum length, and the number and percentage of fish meeting the statewide or other specified legal minimum size limit.

``` r
catch_summary <- catchSummary(catch)
```

### Catch by effort

`catchByEffort()` summarizes catch associated with individual sampling efforts and includes information such as total catch, average length, minimum and maximum length, and legal-sized catch where applicable.

``` r
catch_by_effort <- catchByEffort(
con = con,
effortData = efforts
)
```

### CPUE

`CPUE_byGear()` summarizes catch per unit effort by sampling gear. OutputTypes include "Table" or "Figure"

``` r
cpue <- CPUE_byGear(
catchData=catch,
OutputType="Table"
)
CPUE_byGear(
catchData=catch,
OutputType="Figure"
)
```



### Age-length summaries

`ageLengthSummary()` summarizes age-length information and can return "RawData", "Table", or "Figure".


``` r
age_length_raw <- ageLengthSummary(
con = con,
effortData = efforts,
OutputType = "RawData"
)

# Return summary statistics
age_length_table <- ageLengthSummary(
con = con,
effortData = effortData,
OutputType = "Table"
)

# Create a length-at-age figure
ageLengthSummary(
con = con,
effortData = effortData,
OutputType = "Figure"
)
```

### Detection by year
`detectionByYear()` produces a table that summarizes which years a given species has been detected. 

``` r
# Query catch data for a waterbody
catchData <- FISH_query(
con,
QueryType = "Catch",
WaterBodyName = "Lake Orion"
)

# Create a yearly detection summary
detection_summary <- detectionByYear(catchData)

```
### Length frequency

`lengthFreqByEffort()` summarizes length-frequency information by survey, sampling module, species, and inch group.

``` r
length_frequency <- lengthFreqByEffort(
con = con,
effortData = efforts
)
```

A figure can also be returned:

``` r
lengthFreqByEffort(
con = con,
effortData = efforts,
OutputType = "Figure"
)
```
### Relative stock density

`RSD()` calculates relative stock density using Gabelhouse length categories.

``` r
rsd <- RSD(length_frequency)
```

## Package functions

| Function               | Purpose                                   |
|------------------------|-------------------------------------------|
| `MFA_connect()`        | Connect to the FISHub database            |
| `FISH_query()`         | Query survey, effort, and catch data      |
| `catchSummary()`       | Summarize catch                           |
| `catchByEffort()`      | Summarize catch by sampling effort        |
| `CPUEbyGear()`         | Calculate catch per unit effort by gear   |
| `lengthFreqByEffort()` | Summarize length-frequency data by effort |
| `ageLengthSummary()`   | Summarize age and length data             |
| `detectionByYear()`    | Summarize species detection by year       |
| `RSD()`                | Calculate relative stock density          |

## Development

MFA is currently under development. The package is being developed to provide a consistent set of tools for querying and analyzing Michigan fisheries data and will eventually support a Shiny application for users who do not need to work directly in R.

## License

MFA is currently under development and the distribution and licensing terms are being finalized.
