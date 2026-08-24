#' CPUEbyGear
#'
#' Calculate catch per unit effort (CPUE) by gear and species
#'
#' Calculates catch per unit effort (CPUE) by survey, gear type, and species
#' using catch and effort data returned by \code{\link{FISH_query}}. The
#' function can return either a summary data frame or a figure showing CPUE
#' by species, with separate panels for each gear type.
#'
#' The function uses \code{EffortTotalQuantity} and
#' \code{EffortTotalMeasurement} as the primary measure of effort when a
#' non-zero total effort is available. If the total effort is missing or zero,
#' the alternate effort fields (\code{EffortAlternateQuantity} and
#' \code{EffortAlternateMeasurement}) are used instead.
#'
#' Alternate effort information is also retained in the output when available.
#' This allows users to compare CPUE calculated using the primary and alternate
#' effort measurements. Effort reported in seconds is converted to minutes
#' before CPUE is calculated.
#'
#' Records with both primary and alternate effort quantities missing are
#' excluded from the CPUE calculation. A warning message identifies the
#' number of effort records removed. Records identified as \code{"LIMNO"} are
#' excluded from this warning because limnological sampling records may not
#' represent fishing effort.
#'
#' @param catchData A data frame containing catch and effort data,
#'   typically returned by \code{\link{FISH_query}} with
#'   \code{QueryType = "Catch"}. The data must contain the catch, effort,
#'   survey, waterbody, and gear fields required by this function, including
#'   \code{SurveyId}, \code{WaterBodyName}, \code{SurveyEffortId},
#'   \code{SurveyEffortKey}, \code{GearType}, \code{Species},
#'   \code{TotalNumberCaught}, \code{EffortNumberofGearUsed},
#'   \code{EffortTotalQuantity}, \code{EffortTotalMeasurement},
#'   \code{EffortAlternateQuantity}, and
#'   \code{EffortAlternateMeasurement}.
#'
#' @param OutputType Character string specifying the type of output to
#'   return. Must be either \code{"Table"} or \code{"Figure"}. The default is
#'   \code{"Table"}.
#'
#' @return If \code{OutputType = "Table"}, a data frame containing CPUE
#'   summaries by waterbody, survey, gear type, and species. The returned
#'   data frame contains the following columns:
#'   \describe{
#'     \item{WaterBodyName}{Name of the waterbody associated with the survey.}
#'     \item{SurveyId}{FISHub survey identifier.}
#'     \item{GearType}{Gear type used to collect the catch.}
#'     \item{Species}{Species associated with the catch record.}
#'     \item{TotalNumberCaught}{Total number of fish caught for the
#'       survey, gear type, and species combination.}
#'     \item{TotalEffort}{Total primary effort used to calculate CPUE.
#'       Effort reported in seconds is converted to minutes.}
#'     \item{Unit}{Unit associated with \code{TotalEffort}. When the
#'       original unit was seconds, the output unit is changed to minutes.}
#'     \item{CPUE}{Catch per unit effort calculated as
#'       \code{TotalNumberCaught / TotalEffort}.}
#'     \item{TotalEffort_alt}{Total alternate effort measurement. When
#'       available, effort reported in seconds is converted to minutes.}
#'     \item{Unit_alt}{Unit associated with \code{TotalEffort_alt}. When the
#'       original unit was seconds, the output unit is changed to minutes.}
#'     \item{CPUE_alt}{Catch per unit effort calculated using the alternate
#'       effort measurement.}
#'   }
#'
#'   If \code{OutputType = "Figure"}, a \code{ggplot} object showing CPUE by
#'   species. Separate panels are created for each gear type, and surveys
#'   are displayed as separate bars.
#'
#' @details
#' Catch is first summarized by \code{SurveyId}, \code{GearType}, and
#' \code{Species}. Effort is summarized by waterbody, survey, gear type, and
#' effort units before the catch and effort summaries are joined.
#'
#' The primary effort measure is selected using the following rule:
#'
#' \itemize{
#'   \item If \code{EffortTotalQuantity} is non-missing and non-zero, it is
#'     used as the primary effort measure.
#'   \item If \code{EffortTotalQuantity} is missing or zero,
#'     \code{EffortAlternateQuantity} is used as the primary effort measure.
#'   \item The corresponding measurement field determines the unit of the
#'     selected effort value.
#' }
#'
#' The alternate effort value is retained as \code{Effort_alt} and is used to
#' calculate \code{CPUE_alt}. This provides a second CPUE estimate when the
#' data contain an alternate effort measurement.
#'
#' Effort reported in seconds is converted to minutes by dividing the effort
#' by 60. Thus, CPUE values associated with effort originally recorded in
#' seconds are expressed as catch per minute rather than catch per second.
#'
#' Only records with a non-missing \code{Species} are included in the catch
#' summary. Effort records with both primary and alternate effort quantities
#' missing are excluded from the analysis. The function reports the number
#' of excluded effort records using a warning message.
#'
#' CPUE is calculated as:
#'
#' \deqn{CPUE = \frac{TotalNumberCaught}{TotalEffort}}
#'
#' and alternate CPUE is calculated as:
#'
#' \deqn{CPUE_{alt} = \frac{TotalNumberCaught}{TotalEffort_{alt}}}
#'
#' The function does not standardize CPUE across different gear types. CPUE
#' values should therefore generally be interpreted within gear type unless
#' the underlying effort units and sampling methods are directly comparable.
#'
#' @examples
#' \dontrun{
#' # Calculate CPUE for a single survey
#' catchData <- FISH_query(
#'   con,
#'   QueryType = "Catch",
#'   SurveyId = 1162
#' )
#'
#' CPUE <- CPUEbyGear(catchData)
#'
#' # Calculate CPUE for multiple Status & Trends surveys
#' catchData <- FISH_query(
#'   con,
#'   QueryType = "Catch",
#'   SurveyPurpose = "Status & Trends",
#'   Year = 2025
#' )
#'
#' CPUE <- CPUEbyGear(catchData)
#'
#' # Return a CPUE figure instead of a table
#' CPUE_plot <- CPUEbyGear(
#'   catchData,
#'   OutputType = "Figure"
#' )
#' }
#'
#' @export


CPUEbyGear<-function(catchData,OutputType="Table"){
  
  if (!OutputType %in% c("Table", "Figure")) {
    stop("OutputType must be either 'Table' or 'Figure'.")
  }
  
  #get efforts with NAs in effort quantity column for removal (e.g., limno or missing data)
  effortsNA<-catchData%>%
    filter(is.na(EffortTotalQuantity)&is.na(EffortAlternateQuantity))%>%
    filter(trimws(GearType)!="LIMNO")

  #print warning
  if(nrow(effortsNA)>0){
    message(paste0("WARNING:",nrow(effortsNA)," effort(s) removed from summary due to missing data. Look at raw output to determine missing data."))
  }

  #clean up data
  catchDataClean<-catchData%>%
    #remove missing efforts (a warning will spit out above)
    filter(!is.na(EffortTotalQuantity) | !is.na(EffortAlternateQuantity))%>%
    #clean up effort units and values
    mutate(
      #use the EffortTotalQuantity and EffortTotalMeasurement as primary, if exists; otherwise pull from alternate
      use_total = !is.na(EffortTotalQuantity) & EffortTotalQuantity != 0,
      Effort = if_else(
        use_total,
        EffortTotalQuantity,
        EffortAlternateQuantity
      ),
      
      Unit = case_when(
        use_total ~ EffortTotalMeasurement,
        TRUE      ~ EffortAlternateMeasurement
      ),
      
      #use alterante unit and value, if not used for primary
      Effort_alt = case_when(
        use_total ~ EffortAlternateQuantity,
        TRUE      ~ EffortTotalQuantity
      ),
      
      Unit_alt = case_when(
        use_total ~ EffortAlternateMeasurement,
        TRUE      ~ EffortTotalMeasurement
      )
    )%>%
    select(-c(EffortTotalQuantity,EffortTotalMeasurement,EffortAlternateQuantity,EffortAlternateMeasurement,use_total))
    
  #summarize catch data
  catchByGear<-catchDataClean%>%
    filter(!is.na(Species))%>%
    group_by(SurveyId,GearType,Species)%>%
    summarise(TotalNumberCaught=sum(TotalNumberCaught,na.rm = T),.groups = "drop")
  
  #summarize effort data
  effortByGear<-catchDataClean%>%
    select(WaterBodyName,SurveyId,SurveyEffortId,SurveyEffortKey,GearType,EffortNumberofGearUsed,Effort,Unit,Effort_alt,Unit_alt)%>%
    unique()%>%
    group_by(WaterBodyName,SurveyId,GearType,Unit,Unit_alt)%>%
    summarize(TotalEffort=sum(as.numeric(Effort)),
              TotalEffort_alt=sum(as.numeric(Effort_alt)),
              .groups = "drop")
  
  
  #merge tables and calculate CPUE
  allDat<-effortByGear%>%
    inner_join(catchByGear,by=c("SurveyId","GearType"))%>%
    mutate(
      TotalEffort = if_else(
        Unit == "Seconds",
        TotalEffort / 60,
        TotalEffort
      ),
      TotalEffort_alt = if_else(
        Unit_alt == "Seconds",
        TotalEffort_alt / 60,
        TotalEffort_alt
      ),
      Unit = if_else(
        Unit == "Seconds", "Minutes", Unit
      ),
      Unit_alt = if_else(
        Unit_alt == "Seconds", "Minutes", Unit_alt
      ),
      CPUE=TotalNumberCaught/TotalEffort,
      CPUE_alt=TotalNumberCaught/TotalEffort_alt)%>%
    select(WaterBodyName,SurveyId,GearType,Species,TotalNumberCaught,
           TotalEffort,Unit,CPUE,
           TotalEffort_alt,Unit_alt,CPUE_alt)
  
  if(OutputType=="Table"){
    return(allDat)
  }
  
  if(OutputType=="Figure"){
    plotOut<-ggplot(allDat)+
      geom_bar(aes(x=Species,y=CPUE,fill=as.factor(SurveyId)),stat="identity",position = "dodge")+
      facet_wrap(~GearType,scales = "free")+
      theme_classic()+
      scale_fill_viridis_d()+
      xlab("Species")+ylab("CPUE (net lift or seconds E-fishing)")+labs(fill="Survey")+
      theme(
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)
      )
    return(plotOut)
    
  }
}