#' RSD
#'
#' Creates summary table or graph of RSD values based on Gabelhouse length categories
#'
#' @param lengthFreqData Data query from lengthFreqByEffort function
#' @return Summary Table
#' @export
#'
#' @details N.Stock means # of fish >=stock length but < Quality length. Total N.Stock = N.Stock+N.Quality+N.Preferred+N.Memorable+N.Trophy


RSD<-function(lengthFreqData){
  
  if(!"TotalNumberCaught" %in% names(lengthFreqData)){
    stop("No catch data included. Need to provide catch data from FISH_query")
  }
  
  #read in gabelhouse lengths-- used relative path for app compatability
  RSDvals<-readr::read_csv("data/Gabelhouse_RSD_lengths.csv",show_col_types = FALSE)
  
  #add gabelhouse categories to length data
  labels<-c("Stock","Quality","Preferred","Memorable","Trophy")
  lengthFreqData$RSDcat<-suppressWarnings(apply(lengthFreqData,1,function(x)
    labels[max(which((as.numeric(x["InchGroup"])>=
                        as.numeric(RSDvals[match(x["Species"],RSDvals$species),4:8]))==T))]))
  
  #RSD summary
  RSDsum<-lengthFreqData%>%
    filter(Species%in%RSDvals$species)%>%
    group_by(SurveyId,Species)%>%
    summarize(N.Stock=sum(TotalNumberCaught[RSDcat=="Stock"],na.rm = T),
              N.Quality=sum(TotalNumberCaught[RSDcat=="Quality"],na.rm = T),
              N.Preferred=sum(TotalNumberCaught[RSDcat=="Preferred"],na.rm = T),
              N.Memorable=sum(TotalNumberCaught[RSDcat=="Memorable"],na.rm = T),
              N.Trophy=sum(TotalNumberCaught[RSDcat=="Trophy"],na.rm = T),
              PSD=round(sum(TotalNumberCaught[RSDcat%in%c("Quality","Preferred","Memorable","Trophy")])/
                          sum(TotalNumberCaught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.p=round(sum(TotalNumberCaught[RSDcat%in%c("Preferred","Memorable","Trophy")])/
                            sum(TotalNumberCaught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.m=round(sum(TotalNumberCaught[RSDcat%in%c("Memorable","Trophy")])/
                            sum(TotalNumberCaught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.t=round(sum(TotalNumberCaught[RSDcat%in%c("Trophy")])/
                            sum(TotalNumberCaught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              .groups = "drop")%>%
    
    as.data.frame()
  
  return(RSDsum)
}