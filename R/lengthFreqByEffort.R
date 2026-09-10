#' lengthFreqByEffort
#'
#' Summarize length-frequency distributions by sampling effort
#'
#' Creates a summary of fish length-frequency data by survey, sampling
#' module, species, and inch group using length-group catch data from FISHub.
#' The function can return either a summary table or a figure showing the
#' length-frequency distribution by species.
#'
#' Length-frequency information is obtained primarily from the
#' \code{ModuleDataCatchSampleByInchGroup} table. The function uses the number
#' of unmarked fish caught in each inch group and excludes marked fish from
#' the catch count to avoid double-counting fish.
#'
#' The function also checks for records in the
#' \code{ModuleDataScaleEnvelope} table. Scale-envelope data are not currently
#' incorporated into the returned length-frequency summary; if such records
#' are present, a message is printed indicating that the MFA team should be
#' contacted.
#'
#' @param con A \code{DBI} database connection to the FISHub database. The
#'   connection must be active when the function is called. The function
#'   queries the \code{ModuleDataCatchSampleByInchGroup},
#'   \code{ModuleDataScaleEnvelope}, \code{ModuleData},
#'   and \code{SpeciesStrain} tables through this connection.
#'
#' @param effortData A data frame containing survey and effort
#'   information, typically returned by \code{\link{FISH_query}} with an
#'   effort-related query. The data must contain at least \code{SurveyId} and
#'   \code{ModuleId}. The \code{ModuleId} values are used to identify the
#'   catch records to include, while \code{SurveyId} is used to associate
#'   catch records with the corresponding survey.
#'
#' @param OutputType Character string specifying the type of output to
#'   return. Must be either \code{"Table"} or \code{"Figure"}. The default is
#'   \code{"Table"}.
#'
#' @return If \code{OutputType = "Table"}, a data frame containing length
#'   frequency summaries with the following columns:
#'   \describe{
#'     \item{SurveyId}{FISHub survey identifier.}
#'     \item{ModuleId}{FISHub sampling module identifier.}
#'     \item{Species}{Common species name from the FISHub
#'       \code{SpeciesStrain} table.}
#'     \item{InchGroup}{Length-group identifier from the FISHub catch data.
#'       The value represents the inch group in which the fish were recorded.}
#'     \item{TotalNumberCaught}{Total number of unmarked fish recorded in
#'       the corresponding survey, module, species, and inch group.}
#'     \item{CatchTable}{Identifies the source of the summary. Currently,
#'       returned records are identified as \code{"InchGroup"}.}
#'   }
#'
#'   If \code{OutputType = "Figure"}, a \code{ggplot} object showing the
#'   number of fish caught in each inch group. Species are displayed in
#'   separate facets and surveys are represented by different fill groups.
#'
#' @details
#' The function uses the \code{ModuleId} values in \code{effortData} to
#' restrict the FISHub catch records included in the analysis. Catch records
#' are then linked back to \code{SurveyId} using the survey-effort data.
#'
#' Only data from \code{ModuleDataCatchSampleByInchGroup} are currently used
#' to construct the returned length-frequency distribution. The
#' \code{ModuleDataCatchBySpecies} table is not used because it does not
#' contain individual inch-group information.
#'
#' For each survey, module, species, and inch group, the function sums
#' \code{NumberCaughtUnmarked}. Marked fish are not included in
#' \code{TotalNumberCaught} because marked fish may also be represented in the
#' unmarked catch count and including both could result in double-counting.
#'
#' The function does not currently calculate or return the midpoint length
#' of each inch group. For analyses requiring a continuous length estimate,
#' the midpoint can be approximated as \code{InchGroup + 0.5}, consistent with
#' the approach used elsewhere in the package.
#'
#' The output retains \code{ModuleId}, allowing length-frequency information
#' to be associated with individual sampling modules or efforts. Records are
#' not combined across modules in the returned table.
#'
#' If no inch-group catch records are available for the requested modules,
#' the resulting summary will contain no catch records. Scale-envelope data
#' are currently not used as an alternative source of length-frequency
#' information.
#'
#' @examples
#' \dontrun{
#' #connect to database
#' con <- FISHub_connect()
#' 
#' # Query survey effort data
#' effortData <- FISH_query(
#'   con,
#'   QueryType = "Efforts",
#'   SurveyId = 1162
#' )
#'
#' # Return length-frequency table
#' length_frequency <- lengthFreqByEffort(
#'   con = con,
#'   effortData = effortData
#' )
#'
#' # Create a length-frequency figure
#' length_frequency_plot <- lengthFreqByEffort(
#'   con = con,
#'   effortData = effortData,
#'   OutputType = "Figure"
#' )
#' 
#' #disconnect from database
#' DBI::dbDisconnect(con)
#' }
#'
#' @importFrom dplyr tbl select left_join filter collect mutate group_by summarize bind_rows '%>%'
#' @importFrom ggplot2 ggplot geom_col aes theme_classic facet_wrap labs
#' 
#' @export

lengthFreqByEffort<-function(con,effortData,OutputType="Table"){
  
  if (!OutputType %in% c("Table", "Figure")) {
  stop("OutputType must be either 'Table' or 'Figure'.")
}
  #pull moduleIds from the data
  modIds <- unique(effortData$ModuleId)
  
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
        count = NumberCaughtUnmarked) %>% #only use unmarked here to avoid double counting fish
      group_by(SurveyId,ModuleId,SpeciesStrainId,InchGroup) %>%
      summarize(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        CatchTable = "InchGroup",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #individual data 
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthInInches)%>%
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
    #   summarize(TotalLengthInInches =mean(TotalLengthInInches ,na.rm = T), .groups = "drop")%>%
    #   group_by(SurveyId,SpeciesStrainId,LegalSize) %>%
    #   summarize(
    #     TotalNumberCaught = sum(n(), na.rm = TRUE),
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
    inch    = if (exists("catchInchSum")) catchInchSum else NULL
    #scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL #add this back in whenever we have data in this table
  )

  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])%>%
    left_join(SpeciesStrain,by = "SpeciesStrainId")%>%
    select(SurveyId,ModuleId,Species,InchGroup,TotalNumberCaught,CatchTable)
  
  if(OutputType=="Table"){
    return(combinedSum)
  }
  if(OutputType=="Figure"){
    return(ggplot(combinedSum)+geom_col(aes(x=InchGroup,y=TotalNumberCaught,fill=as.factor(SurveyId)))+
             theme_classic()+
             facet_wrap(~Species)+
             labs(fill = "SurveyId"))
  }
}