#' detectionByYear
#'
#' Creates summary table of detections by species by year

#'
#' @param catchData Data query from FISH_query function
#' @return Summary Table
#' @export
#'


detectionByYear <- function(catchData) {
  
  #check for multiple waterbodies
  if(n_distinct(catchData$MDNRID)>1){
    message("WARNING: yearly detection summary includes multiple waterbodies")
  }
  
  #get detections by species and year
  detectSummary<-catchData%>%
    filter(!is.na(GearType))%>% #removes limno
    mutate(Year=year(SurveyBeginTimestamp))%>%
    group_by(Year,Species)%>%
    summarise(Detect=as.integer(sum(TotalNumberCaught,na.rm = T)>0),.groups = "drop")%>%
    #mutate(Detect="x",Species=as.character(varhandle::unfactor(Species)))%>%
    mutate(Detect="x")%>%
    tidyr::spread(key=Year,value = Detect)%>%
    as.data.frame()%>%
    filter(!is.na(Species)) #surveys with no catch will show up as NA
  
  # for(i in 2:ncol(detectSummary)){
  #   detectSummary[which(is.na(detectSummary[,i])),i]<-""
  # }
  
  #get # surveys and # efforts by year
  effortSummary<-catchData%>%
    filter(!is.na(GearType))%>%
    mutate(Year=year(SurveyBeginTimestamp))%>%
    group_by(Year)%>%
    summarise(N_Surveys=length(unique(SurveyId)),
              N_Efforts=length(unique(paste0(SurveyId,SurveyEffortKey))))%>%
    as.data.frame()%>%
    t()
  
  #add effort data to detection table
  detectSummary[(nrow(detectSummary)+1),]<-c(row.names(effortSummary)[2],effortSummary[2,])
  detectSummary[(nrow(detectSummary)+1),]<-c(row.names(effortSummary)[3],effortSummary[3,])
  
  
  
  return(detectSummary)
}


