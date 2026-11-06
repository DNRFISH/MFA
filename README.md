
<!-- README.md is generated from README.Rmd. Edit README.Rmd and the shared tutorial instead. -->

# Michigan Fisheries Analysis (MFA)

Michigan Fisheries Analysis (MFA) is an R package for querying and
summarizing fisheries data from the Michigan Department of Natural
Resources FISHub database. It supports retrieval of survey, effort, and
catch data and analyses such as catch summaries, catch per unit effort
(CPUE), length-frequency distributions, age-length summaries, species
detection, and relative stock density (RSD).

MFA can be used through a Shiny application or directly through R
functions.

## Development

MFA is under active development. Results may contain errors. Please
review outputs carefully and notify Lucas Nathan
(<nathanl@michigan.gov>) if you identify a potential error. Planned
expansions include additional FISHub modules (such as stocking), other
fisheries datasets (such as Master Angler), and external data sources.

## Installation

MFA is distributed internally to Michigan DNR staff through the shared
MFA installation folder on SharePoint. Staff should follow the
installation instructions provided with that folder.

Authorized developers with access to the private repository can install
a tagged release from GitHub:

``` r
remotes::install_github("DNRFISH/MFA@v2.0.0")
```

## Shiny application

Launch the application with:

``` r
MFA::run_MFA_app()
```

## Using MFA functions in R

After installing MFA, open this tutorial from R:

``` r
vignette("MFA-introduction", package = "MFA")
```

The examples below demonstrate how to connect to FISHub, query data, and
generate fisheries summaries. They require access to the FISHub database
and are shown without executing the queries during documentation builds.

## Connecting to FISHub

MFA uses Windows integrated authentication to connect to the FISHub
database. Load MFA and establish a connection before running database
queries:

``` r
library(MFA)
con <- FISHub_connect()
```

## Function reference

| Function               | Purpose                                   |
|------------------------|-------------------------------------------|
| `FISHub_connect()`     | Connect to the FISHub database            |
| `FISH_query()`         | Query survey, effort, and catch data      |
| `catchSummary()`       | Summarize catch                           |
| `CPUE_byGear()`        | Calculate catch per unit effort by gear   |
| `ageLengthSummary()`   | Summarize age and length data             |
| `detectionByYear()`    | Summarize species detection by year       |
| `lengthFreqByEffort()` | Summarize length-frequency data by effort |
| `RSD()`                | Calculate relative stock density          |

For detailed arguments and return values, use the function help pages,
such as `?FISH_query` or `?RSD`, after loading MFA.

## Querying FISHub

`FISH_query()` retrieves survey, effort, or catch data. For example,
retrieve Status & Trends surveys from 2025:

``` r
surveys <- FISH_query(
  con,
  QueryType = "Survey",
  SurveyCategory = "Status & Trends",
  Year = 2025
)
```

Retrieve effort data for a specific survey:

``` r
efforts <- FISH_query(
  con,
  QueryType = "Efforts",
  SurveyId = 805
)
```

Retrieve catch data filtered by survey and gear:

``` r
catch <- FISH_query(
  con,
  QueryType = "Catch",
  SurveyId = 805,
  GearType = "SMFYKE"
)
```

Catch data include the number and percentage of fish meeting the
applicable minimum length. MFA uses default statewide minimum size
limits where available; specify an alternative using
`Special_Legal_Sizes`:

``` r
catch <- FISH_query(
  con,
  QueryType = "Catch",
  SurveyId = 805,
  Special_Legal_Sizes = c("Largemouth Bass" = 10)
)
```

## Fisheries summaries

### Catch summary

`catchSummary()` returns one row per survey and species, including total
catch, weighted mean length, minimum and maximum length, and the number
and percentage meeting the applicable minimum size limit.

``` r
catch_summary <- catchSummary(catch)
```

### Catch per unit effort (CPUE)

`CPUE_byGear()` summarizes catch per unit effort by sampling gear and
can return a table or figure:

``` r
cpue <- CPUE_byGear(catchData = catch, OutputType = "Table")
CPUE_byGear(catchData = catch, OutputType = "Figure")
```

### Age-length summaries

`ageLengthSummary()` provides raw age-length data, summary statistics,
or a length-at-age figure:

``` r
age_length_raw <- ageLengthSummary(
  con = con,
  effortData = efforts,
  OutputType = "RawData"
)

age_length_table <- ageLengthSummary(
  con = con,
  effortData = efforts,
  OutputType = "Table"
)

ageLengthSummary(
  con = con,
  effortData = efforts,
  OutputType = "Figure"
)
```

### Detection by year

`detectionByYear()` identifies the years in which each species was
detected in the queried catch data:

``` r
catchData <- FISH_query(
  con,
  QueryType = "Catch",
  WaterBodyName = "Lake Orion"
)

detection_summary <- detectionByYear(catchData)
```

### Length frequency

`lengthFreqByEffort()` summarizes length frequencies by survey, sampling
module, species, and inch group. It can return a table or figure:

``` r
length_frequency <- lengthFreqByEffort(
  con = con,
  effortData = efforts,
  OutputType = "Table"
)

lengthFreqByEffort(
  con = con,
  effortData = efforts,
  OutputType = "Figure"
)
```

### Relative stock density

`RSD()` calculates relative stock density using Gabelhouse length
categories. For example, RSD-Trophy is calculated as:

$$RSD\text{-}T =
\frac{N_{Trophy}}
{N_{Stock} + N_{Quality} + N_{Preferred} + N_{Memorable} + N_{Trophy}}
\times 100$$

Here, each $N$ is the number of fish within the indicated, mutually
exclusive length category (Stock, Quality, Preferred, Memorable, or
Trophy).

``` r
rsd <- RSD(length_frequency)
```

## Closing the database connection

Disconnect when you have finished querying FISHub:

``` r
DBI::dbDisconnect(con)
```

## License

MFA is currently intended for internal Michigan DNR use. Distribution
and licensing terms are being finalized.
