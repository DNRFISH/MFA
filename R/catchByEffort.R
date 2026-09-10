#' catchByEffort
#'
#' Summarize fish catches by survey, species, and sampling effort
#'
#' Creates a standardized summary of fish catches from the FISHub database,
#' combining catch information from species-level and inch-group catch tables.
#' Results are summarized by survey, effort, and species/strain and include
#' total catch, length statistics, and the number of fish estimated to be at
#' or above the applicable legal size when individual or inch-group length
#' information is available.
#'
#' The function uses the \code{ModuleId} values contained in \code{effortData}
#' to identify the catch records to summarize. Catch data are obtained from
#' the \code{ModuleDataCatchBySpecies} and
#' \code{ModuleDataCatchSampleByInchGroup} FISHub tables.
#'
#' For species-level catch records, total catch is summed across records and
#' average length is calculated as a catch-weighted mean of the reported
#' average lengths. Minimum and maximum lengths are calculated from the
#' reported minimum and maximum values. Because species-level records do not
#' contain individual fish lengths, the number of fish at or above the legal
#' size cannot be calculated and is returned as \code{NA}.
#'
#' For inch-group catch records, the function uses the number of unmarked fish
#' (\code{NumberCaughtUnmarked}) to avoid double-counting marked fish. Length
#' is estimated as the midpoint of each inch group (e.g., an inch group of
#' 10 is represented as 10.5 inches). Average length is calculated as a
#' catch-weighted mean of these estimated lengths. The number of fish at or
#' above the applicable legal size is calculated from the inch-group data.
#'
#' A set of default legal sizes is included for selected species. These values
#' are intended as statewide defaults and can be replaced for individual
#' species using \code{Special_Legal_Sizes}. Species without a default legal
#' size, or species for which no override is supplied, will have
#' \code{NA} for \code{LegalSize} and \code{N_legal}.
#'
#' Currently, scale-envelope data are checked for but are not included in the
#' returned summary. If scale-envelope records are present, the function
#' prints a message indicating that the MFA team should be contacted.
#'
#' @param con A \code{DBI} database connection to the FISHub database.
#'   The connection must be active when the function is called. The function
#'   queries the \code{SpeciesStrain}, \code{ModuleData},
#'   \code{ModuleDataCatchBySpecies}, \code{ModuleDataCatchSampleByInchGroup},
#'   and \code{ModuleDataScaleEnvelope} tables through this connection.
#'
#' @param effortData A data frame returned by \code{\link{FISH_query}}.
#'   The data must contain \code{ModuleId} and \code{SurveyId} columns, but 
#'   should not include catch data (QueryType ="Efforts").
#'   The \code{ModuleId} values are used to restrict the catch records
#'   retrieved from FISHub, while \code{SurveyId} is used to associate catch
#'   summaries with the corresponding survey.
#'
#' @param Special_Legal_Sizes An optional named numeric vector specifying
#'   species-specific legal-size overrides. Names must correspond to species
#'   names in the FISHub \code{SpeciesStrain} table and values must be legal
#'   sizes in inches. For example,
#'   \code{c("Largemouth Bass" = 10, "Brook Trout" = 12)} will replace the
#'   default legal sizes for those species. Species not included in this
#'   vector retain their default legal size, if one is defined.
#'
#' @return A data frame containing catch summaries. Each row represents a
#'   combination of survey, module, and species, with the following columns:
#'   \describe{
#'     \item{SurveyId}{FISHub survey identifier.}
#'     \item{ModuleId}{FISHub module identifier associated with the catch
#'       records.}
#'     \item{Species}{Common species name from the FISHub
#'       \code{SpeciesStrain} table.}
#'     \item{TotalNumberCaught}{Total number of fish caught in the
#'       summarized records. For inch-group data, unmarked fish are used to
#'       avoid double-counting marked fish.}
#'     \item{LengthAverage}{Average fish length in inches. For species-level
#'       data, this is a catch-weighted mean of reported average lengths. For
#'       inch-group data, it is a catch-weighted mean based on estimated
#'       inch-group midpoints.}
#'     \item{LengthMinimum}{Minimum reported or estimated length in inches.
#'       For species-level data, this is the minimum reported minimum length.
#'       For inch-group data, this is the minimum estimated inch-group
#'       midpoint.}
#'     \item{LengthMaximum}{Maximum reported or estimated length in inches.
#'       For species-level data, this is the maximum reported maximum length.
#'       For inch-group data, this is the maximum estimated inch-group
#'       midpoint.}
#'     \item{LegalSize}{Applicable legal size in inches for the species, based
#'       on the function's default legal-size table or
#'       \code{Special_Legal_Sizes}. May be \code{NA} when no legal size is
#'       specified.}
#'     \item{N_legal}{Number of fish estimated to be at or above
#'       \code{LegalSize}. This can be calculated for inch-group data but is
#'       \code{NA} for species-level data because individual or length-group
#'       information is not available.}
#'     \item{CatchTable}{Identifies the source of the catch summary.
#'       Currently, values are \code{"Species"} for species-level catch data
#'       and \code{"InchGroup"} for inch-group catch data.}
#'   }
#'
#' @details
#' The function does not currently reconcile or combine duplicate catch
#' information that may exist across the species-level and inch-group tables.
#' Instead, records from each source are retained and identified using
#' \code{CatchTable}. This allows the results to be compared during data
#' validation and prevents inadvertent double-counting when the same catch
#' information is represented in multiple source tables.
#'
#' The current length calculations should be interpreted with caution.
#' Average lengths generated by this function may not exactly correspond to
#' lengths reported in Survey Fisheries Reports (SFRs) because of known
#' underlying data issues identified during FISHub development.
#'
#' For inch-group data, the function assumes that the value stored in
#' \code{InchGroup} represents the lower bound of an inch-length interval and
#' estimates fish length using \code{InchGroup + 0.5}. Consequently,
#' \code{N_legal} is an estimate based on length groups rather than a count
#' derived from individual fish measurements.
#'
#' @examples
#' \dontrun{
#' # Query FISHub data
#' effortData<-FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#'
#' # Summarize catches using default legal sizes
#' catch_summary <- catchByEffort(con = con,effortData = effortData)
#'
#' # Override legal sizes for species with waterbody-specific regulations
#' catch_summary <- catchByEffort(con = con,effortData = effortData,Special_Legal_Sizes = c("Largemouth Bass" = 10,"Brook Trout" = 12))
#' }
#'
#'
#' @importFrom dplyr tbl filter select left_join mutate coalesce group_by summarise bind_rows
#' 
#' @export

#NOTE: avg length doesn't currently line up with SFRs because of data issues identifed during FISH development

catchByEffort <- function(con,effortData,Special_Legal_Sizes = NULL) {
  
  if(!"SurveyEffortId" %in% names(effortData)){
    stop("No survey data included. Need to provide effort data from FISH_query [QueryType='Efforts']")
  }
  
  if("TotalNumberCaught" %in% names(effortData)){
    stop("Catch data included. Need to provide effort data from FISH_query [QueryType='Efforts']") #could modify this to pull out efforts
  }
  
  #pull moduleIds from the data
  modIds <- unique(effortData$ModuleId)

  #specify default legal sizes (commented species don't have MSLs)
  legalSizeTib<-c(
    #"Black Crappie"=7,
    "Brook Trout"=7,
    #"Bluegill"=6,
    "Brown Trout"=8,
    #"Brown Bullhead"=7,
    "Channel Catfish"=12,
    "Chinook Salmon"=8,
    "Coho Salmon"=8,
    "Flathead Catfish"=15,
    #"Green Sunfish"=6,
    #"Hybrid Sunfish"=6,
    "Lake Trout"=10,
    "Largemouth Bass"=14,
    #"Longear Sunfish"=6,
    #"Lake Whitefish"=8,
    "Muskellunge"=42,
    "Northern Pike"=24,
    #"Orangespotted Sunfish"=6,
    "Pink Salmon"=7,
    #"Pumpkinseed"=6,
    "Rainbow Trout"=8,
    #"Rock Bass"=6,
    #"Redear Sunfish"=6,
    "Smallmouth Bass"=14,
    "Splake"=10,
    #"Lake Sturgeon"=50,
    "Walleye"=15
    #"Warmouth"=6,
    #"White Crappie"=7,
    #"White Bass"=7,
    #"White Perch"=7,
    #"Yellow Perch"=7,
    #"Yellow Bullhead"=7
  )
  
  #convert the legal sizes to a tibble
  legalSizeTib <- tibble::tibble(
    Species = names(legalSizeTib),
    LegalSize = as.numeric(legalSizeTib)
  )
  
  #incorporate special legal sizes (e.g., if a waterbody has special regs)
  if (!is.null(Special_Legal_Sizes)) {
    override_tbl <- tibble::tibble(
      Species = names(Special_Legal_Sizes),
      LegalSize_override = as.numeric(Special_Legal_Sizes)
    )
    legalSizeTib <- legalSizeTib %>%
      left_join(override_tbl, by = "Species") %>%
      mutate(
        LegalSize = coalesce(LegalSize_override, LegalSize)
      ) %>%
      select(-LegalSize_override)
  }
  
  #join legal sizes to species str table
  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  legalSizeTibSpeciesStr<-SpeciesStrain%>%
    left_join(legalSizeTib,by="Species")
  
  
  #################################################################################
  #catch by species data ####
  #################################################################################
  catchSpecies <- tbl(con, "ModuleDataCatchBySpecies") %>%
    select(ModuleDataId,SpeciesStrainId,TotalNumberCaught,LengthAverage,LengthMaximum,LengthMinimum)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% #this syntax is needed to force R code into SQL for lazy loading
    collect()
  
  if (nrow(catchSpecies)>0) {
    catchSpeciesSum<-catchSpecies%>%
      #add surveyId
      left_join(effortData%>%select(SurveyId,ModuleId),by="ModuleId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId) %>%
      summarise(
        TotalNumberCaught = sum(TotalNumberCaught, na.rm = TRUE),
        #calculated weighted mean lengths -- need to deal with some species not having length data
        LengthAverage = round({
          valid <- !is.na(LengthAverage) & !is.na(TotalNumberCaught) 
          if (any(valid)) {
            weighted.mean(LengthAverage[valid], w = TotalNumberCaught[valid])
          } else {
            NA_real_
          }
        }, 2),
        LengthMinimum = {
          x <- LengthMinimum[!is.na(LengthMinimum)]
          if (length(x) > 0) min(x) else NA_real_
        },
        LengthMaximum = {
          x <- LengthMaximum[!is.na(LengthMaximum)]
          if (length(x) > 0) max(x) else NA_real_
        },
        #Can't calculate # legal; don't have individual fish lengths
        N_legal = NA,
        LegalSize = NA,
        CatchTable = "Species",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #inch group data  ####
  #################################################################################
  catchInch <- tbl(con, "ModuleDataCatchSampleByInchGroup") %>%
    select(ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>%
    collect()
  
  if (nrow(catchInch)>0) {
    catchInchSum<-catchInch%>%
      #add surveyId
      left_join(effortData%>%select(SurveyId,ModuleId),by="ModuleId")%>%
      mutate(
        count = NumberCaughtUnmarked,#only use unmarked here to avoid double counting fish
        lengthEst=InchGroup+0.5) %>%
      left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId,LegalSize) %>%
      summarise(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        LengthAverage = round(weighted.mean(
          lengthEst,
          w = count,
          na.rm = TRUE
        ),2),
        LengthMinimum = min(lengthEst, na.rm = TRUE),
        LengthMaximum = max(lengthEst, na.rm = TRUE),
        N_legal = sum(count[lengthEst >= LegalSize], na.rm = FALSE), #want this to stay NA if no legal size specified
        CatchTable = "InchGroup",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #individual data 
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% 
    collect()
  
  #check to see if there was any data in the query
  #*NOTE: need to verify this with a survey that has some data (issue #5)
  if (nrow(scaleEnvelope)>0) {
    print("New ModuleDataScaleEnvelope data available; Contact MFA team.")
    # #summarize by survey and species
    # scaleEnvelopeSum <- scaleEnvelope %>%
    #   #had to deal with multiple agers- set it up as mean length by serial number
    #   group_by(SurveyId,SpeciesStrainId,EnvelopeSerialNumber)%>%
    #   summarise(TotalLengthEntered=mean(TotalLengthEntered,na.rm = T), .groups = "drop")%>%
    #   left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
    #   group_by(SurveyId,SpeciesStrainId,LegalSize) %>%
    #   summarise(
    #     TotalNumberCaught = sum(n(), na.rm = TRUE),
    #     LengthAverage = round(mean(TotalLengthEntered, na.rm = TRUE),2),
    #     LengthMinimum = min(TotalLengthEntered, na.rm = TRUE),
    #     LengthMaximum = max(TotalLengthEntered, na.rm = TRUE),
    #     N_legal = sum(count[TotalLengthEntered >= LegalSize], na.rm = FALSE), #want this to stay NA if no legal size specified
    #     CatchTable = "ScaleEnvelope",
    #     .groups = "drop"
    #   )
  }
  
  
  #################################################################################
  # combine data
  #################################################################################
  #merge together - skip if there wasn't any data
  #*Note: for now keeping the three CatchTables separate to help with validation- could merge these in the future?
  tables_list <- list(
    species = if (exists("catchSpeciesSum")) catchSpeciesSum else NULL,
    inch    = if (exists("catchInchSum")) catchInchSum else NULL
    #scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL #add this back in whenever we have data in this table
  )
  
  
  #join legal sizes to species str table
  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])%>%
    left_join(SpeciesStrain,by = "SpeciesStrainId")%>%
    select(SurveyId,ModuleId,Species,TotalNumberCaught,LengthAverage,LengthMinimum,LengthMaximum,LegalSize,N_legal,CatchTable)
    
  return(combinedSum)
}


