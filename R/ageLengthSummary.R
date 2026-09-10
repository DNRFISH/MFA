#' ageLengthSummary
#'
#' Summarize fish length at age
#'
#' Summarizes fish length by age, species, and survey using age-length data
#' from FISHub. The function returns either the processed individual
#' age-length observations, a summary table containing simple and
#' catch-weighted mean lengths, or a figure showing mean length at age.
#'
#' Age-length observations are obtained from the FISHub
#' \code{ModuleDataScaleEnvelope} table. When multiple age records are
#' associated with the same fish, identified by \code{SurveyId} and
#' \code{EnvelopeSerialNumber}, the age occurring most frequently for that
#' fish is retained. If multiple ages occur with the same frequency, one age
#' is selected without further weighting.
#'
#' Simple mean length statistics are calculated directly from the
#' age-length observations. When catch-by-inch-group data are available, a
#' second length-at-age estimate is calculated using the Fisheries Division
#' protocol based on the age-length key and the observed length-frequency
#' distribution.
#'
#' @param con A \code{DBI} database connection to the FISHub database. The
#'   connection must be active when the function is called. The function
#'   queries the \code{ModuleDataScaleEnvelope}, \code{AgeClass},
#'   \code{ModuleData}, \code{SpeciesStrain}, and
#'   \code{ModuleDataCatchSampleByInchGroup} tables through this connection.
#'
#' @param effortData A data frame containing survey and effort
#'   information, typically returned by \code{\link{FISH_query}} with an
#'   effort-related query. The data must contain at least \code{SurveyId} and
#'   \code{ModuleId}. \code{SurveyId} identifies the surveys for which
#'   age-length observations are retrieved, while \code{ModuleId} identifies
#'   the sampling modules used to obtain catch-by-inch-group data for the
#'   weighted length-at-age calculation.
#'
#' @param OutputType Character string specifying the type of output to
#'   return. Must be one of \code{"RawData"}, \code{"Table"}, or
#'   \code{"Figure"}. The default is \code{"RawData"}.
#'   \describe{
#'     \item{\code{"RawData"}}{Returns the processed individual age-length
#'       observations used in the analysis.}
#'     \item{\code{"Table"}}{Returns a summary table containing sample size,
#'       simple mean length statistics, and, when catch-by-inch-group data are
#'       available, catch-weighted mean length.}
#'     \item{\code{"Figure"}}{Returns a \code{ggplot} showing mean length at
#'       age with \eqn{\pm 1} standard deviation and separate facets for
#'       species.}
#'   }
#'
#' @return The return value depends on \code{OutputType}.
#'
#' If \code{OutputType = "RawData"}, a data frame containing the processed
#' age-length observations with the following columns:
#' \describe{
#'   \item{SurveyId}{FISHub survey identifier.}
#'   \item{EnvelopeSerialNumber}{Identifier for the individual fish or scale
#'     envelope.}
#'   \item{Species}{Common species name.}
#'   \item{Strain}{Species strain, when available.}
#'   \item{Age}{Age assigned to the fish based on the most frequently
#'     occurring age record.}
#'   \item{TotalLengthInInches}{Fish total length in inches.}
#' }
#'
#' If \code{OutputType = "Table"}, a data frame summarized by survey,
#' species, and age. The table contains:
#' \describe{
#'   \item{SurveyId}{FISHub survey identifier.}
#'   \item{Species}{Common species name.}
#'   \item{Age}{Fish age in years.}
#'   \item{N}{Number of age-length observations used to calculate the simple
#'     mean.}
#'   \item{Mean_Length}{Simple mean total length in inches.}
#'   \item{Min_length}{Minimum observed total length in inches.}
#'   \item{Max_Length}{Maximum observed total length in inches.}
#'   \item{SD_Length}{Standard deviation of total length in inches.}
#'   \item{Weighted_Mean_Length}{Catch-weighted mean length in inches,
#'     calculated using the observed length-frequency distribution and the
#'     age-length key. This value is only available when compatible
#'     catch-by-inch-group data are present.}
#' }
#'
#' If \code{OutputType = "Figure"}, a \code{ggplot} object showing weighted
#' mean length at age. Points are connected by survey, error bars represent
#' \eqn{\pm 1} standard deviation, and species are displayed in separate
#' facets.
#'
#' @details
#' The function uses \code{SurveyId} values in \code{effortData} to
#' identify the surveys for which age-length observations should be
#' retrieved. Catch-by-inch-group data are restricted using the
#' \code{ModuleId} values in \code{effortData}.
#'
#' Before calculating length-at-age summaries, the function checks for
#' inconsistent lengths among records sharing the same
#' \code{SurveyId} and \code{EnvelopeSerialNumber}. If more than one unique
#' length is associated with an individual fish, the function stops with an
#' error rather than proceeding with potentially inconsistent data.
#'
#' When multiple age records are associated with the same fish, the function
#' determines the most frequently occurring age for each combination of
#' survey, fish, species, strain, and length. This is used to select a single
#' age for the fish. The current implementation does not apply a biological
#' or statistical rule to resolve ties between ages with equal frequencies;
#' one age is selected.
#'
#' The simple length-at-age summary is calculated directly from the processed
#' age-length observations. For each survey, species, and age, the function
#' calculates sample size, mean, minimum, maximum, and standard deviation of
#' total length.
#'
#' When catch-by-inch-group data are available, the function also calculates
#' a catch-weighted mean length at age. The weighted calculation uses:
#'
#' \enumerate{
#'   \item The catch length-frequency distribution from
#'     \code{ModuleDataCatchSampleByInchGroup}.
#'   \item The proportion of aged fish at each inch group and age, which
#'     serves as the age-length key.
#'   \item The number of unmarked fish caught in each inch group to weight
#'     the age-length key.
#' }
#'
#' Only \code{NumberCaughtUnmarked} is used when constructing the length
#' frequency distribution to avoid double-counting marked fish.
#'
#' The age-length key is constructed by assigning each individual fish to
#' an inch group using the floor of its observed total length. The proportion
#' of aged fish belonging to each age within an inch group is then applied to
#' the observed catch in that inch group. The resulting relative numbers are
#' used to calculate the catch-weighted mean length for each age.
#'
#' If no catch-by-inch-group data are available for the requested modules, a
#' warning is issued and the table output contains only the simple
#' length-at-age statistics.
#'
#' The weighted mean length should be interpreted as an estimate of the mean
#' length at age for the sampled catch distribution. It may differ from the
#' simple mean calculated from the aged fish because the weighted calculation
#' accounts for the distribution of the catch among length groups.
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
#' # Return processed individual age-length observations
#' age_length_raw <- ageLengthSummary(
#'   con = con,
#'   effortData = effortData,
#'   OutputType = "RawData"
#' )
#'
#' # Return summary statistics
#' age_length_table <- ageLengthSummary(
#'   con = con,
#'   effortData = effortData,
#'   OutputType = "Table"
#' )
#'
#' # Create a length-at-age figure
#' age_length_plot <- ageLengthSummary(
#'   con = con,
#'   effortData = effortData,
#'   OutputType = "Figure"
#' )
#' 
#' #disconnect from database
#' DBI::dbDisconnect(con)
#' }
#'
#' @importFrom dplyr tbl filter select left_join group_by summarize slice_max mutate ungroup arrange '%>%'
#' @importFrom ggplot2 ggplot geom_line aes geom_errorbar facet_wrap theme_classic ylab xlab guides guide_legend
#' @importFrom stats sd
#'
#' @export

ageLengthSummary<-function(con,effortData,OutputType="Table"){
  
  if (!OutputType %in% c("RawData", "Table", "Figure")) {
    stop("OutputType must be one of 'RawData', 'Table', or 'Figure'.")
  }
  
  # #count the number of fish (SurveyId+EnvelopeSerialNumber) with multiple entries, subset out
  # #based on the initial reveiw there are 2,940 with multiple entires, but they are all identical age and length.. seems suspicious? #8
  # multiEntries <- tbl(con, "ModuleDataScaleEnvelope") %>%
  #   select(SurveyId, EnvelopeSerialNumber, TotalLengthInInches, AgeClassId) %>%
  #   group_by(SurveyId, EnvelopeSerialNumber) %>%
  #   summarize(
  #     numEntry = n(),
  #     n_lengths = n_distinct(TotalLengthInInches),
  #     n_ages=n_distinct(AgeClassId),
  #     .groups = "drop"
  #   ) %>%
  #   filter(numEntry > 1) %>%
  #   inner_join(tbl(con, "ModuleDataScaleEnvelope"),by = c("SurveyId", "EnvelopeSerialNumber"))%>%
  #   collect()
  
  
  #note- currently pulls all age data from surveys and inch group data from all efforts in query
  #pull moduleIds from the data; if not provided, pull by surveyIDs
  SurveyIds <- unique(effortData$SurveyId)
  modIds <- unique(effortData$ModuleId)
  

  
  #do a check for inconsistent lengths; this could happen when multiple ages are entered in FISHub
  #none found when the function was developed (see above multiEntries table)
  length_check <- tbl(con, "ModuleDataScaleEnvelope") %>%
    filter(SurveyId %in% !!SurveyIds) %>% 
    collect()%>%
    group_by(SurveyId, EnvelopeSerialNumber) %>%
    summarize(
      n_lengths = n_distinct(TotalLengthInInches, na.rm = TRUE),
      .groups = "drop"
    )%>%
    filter(n_lengths>1)
  
  if (nrow(length_check) > 0) {
    stop(paste0("ERROR: Inconsistent lengths detected. Check age/length data."))
  }
  
  #query data; currently set up to do mode by SurveyId,EnvelopeSerialNumber,Species,Strain,and TotalLengthInInches
  #circle back to this once we confirm the age data (#8)
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    filter(SurveyId %in% !!SurveyIds) %>% 
    filter(!is.na(TotalLengthInInches))%>% #do we want to filter these? revist after age data confirmation
    select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthInInches,AgeClassId)%>%
    left_join(tbl(con, "AgeClass")%>%
                select(AgeClassId,Descriptions),by = "AgeClassId")%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthInInches,Descriptions)%>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthInInches)%>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    select(SurveyId,EnvelopeSerialNumber,Species,Strain,Age = Descriptions,TotalLengthInInches)%>%
    collect()
  
  if(OutputType=="RawData"){
    return(scaleEnvelope)
  }
  
  
  #Simple mean
  meanAgeDat<-scaleEnvelope%>%
    group_by(SurveyId,Species,Age)%>%
    summarize(N=length(TotalLengthInInches),
              Mean_Length=round(mean(TotalLengthInInches),2),
              Min_length=min(TotalLengthInInches),
              Max_Length=max(TotalLengthInInches),
              SD_Length=round(sd(TotalLengthInInches),2),
              .groups = "drop")
  
  
  #Weighted mean (FD protocol; chap 15 by Schneider)
  #get catch by inch data
  catchInch <- tbl(con, "ModuleDataCatchSampleByInchGroup") %>%
    select(ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>%
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    collect()

  #check to see if there was any data in the query
  if (nrow(catchInch)>0) {
    #join with survey/effort data
    catchInchSurvey<-effortData%>%
      select(SurveyId,ModuleId)%>%
      unique()%>%
      left_join(catchInch,by="ModuleId")
    
    #summarize by survey and species
    lengthFreq <- catchInchSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      group_by(SurveyId,Species,InchGroup) %>%
      summarize(N.caught=sum(NumberCaughtUnmarked),.groups = "drop")#only use unmarked here to avoid double counting fish
      
    alk_prop<-scaleEnvelope%>%
      mutate(InchGroup=floor(TotalLengthInInches))%>%
      group_by(SurveyId,Species,InchGroup,Age)%>%
      summarize(N.aged=n(),
                meanLength=mean(TotalLengthInInches),
                .groups = "drop")%>%
      group_by(SurveyId, Species, InchGroup) %>%
      mutate(prop = N.aged / sum(N.aged)) %>%
      ungroup()
      
    ageLen <- lengthFreq %>%
      inner_join(alk_prop, by = c("SurveyId", "Species", "InchGroup")) %>%
      mutate(relNumber = N.caught * prop,
             relNumber_meanLength=relNumber*meanLength)%>%
       group_by(SurveyId, Species, Age) %>%
       summarize(
         Weighted_Mean_Length = round(sum(relNumber_meanLength)/sum(relNumber),2),
         .groups = "drop"
        )
    
    #combine weighted avg and simple
    outTab<-meanAgeDat%>%
      left_join(ageLen,by= c("SurveyId", "Species", "Age"))%>%
      mutate(Age=as.integer(Age))%>%
      arrange(Species,Age)
    
    if(OutputType=="Table"){
        return(outTab)
    }
  }else{
    warning("No catch-by-inch group data available; returning simple means only.")
    if(OutputType=="Table"){
      return(meanAgeDat)
    }
  }
  if(OutputType=="Figure"){
    outPlot<-ggplot(outTab)+
      geom_line(aes(x=Age,y=Weighted_Mean_Length,group=SurveyId,color=as.factor(SurveyId)))+
      geom_errorbar(aes(x=Age,ymin=Weighted_Mean_Length-SD_Length,ymax=Weighted_Mean_Length+SD_Length,color=as.factor(SurveyId)),width=.5)+
      facet_wrap(~Species)+
      theme_classic()+
      ylab("Mean Length +/- 1 SD (in)")+xlab("Age (years)")+
      guides(color=guide_legend(title="Survey Number"))
    return(outPlot)
  }
}
