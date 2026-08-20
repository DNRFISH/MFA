#' RSD
#'
#' Creates summary table or graph of RSD values based on Gabelhouse length categories
#'
#' @param lengthFreqData Data query from lengthFreqbyEffort function
#' @return Summary Table
#' @export
#'


RSD<-function(lengthFreqData){
  
  if(!"TotalNumberCaught" %in% names(lengthFreqData)){
    stop("No catch data included. Need to provide catch data from FISH_query")
  }
  
  #read in gabelhouse lengths-- used relative path for app compatability
  RSDvals<-readr::read_csv("data/Gabelhouse_RSD_lengths.csv")
  
  #add gabelhouse categories to length data
  labels<-c("Stock","Quality","Preferred","Memorable","Trophy")
  lengthDatSub$RSDcat<-suppressWarnings(apply(lengthDatSub,1,function(x)
    labels[max(which((as.numeric(x["Inch_Group"])>=
                        as.numeric(RSDvals[match(x["Species_Code"],RSDvals$Species_Code),3:7]))==T))]))
  
  #RSD summary
  RSDsum<-lengthDatSub%>%
    filter(!is.na(RSDcat))%>%
    group_by(Survey_Number,Species_Name)%>%
    summarize(N.Stock=sum(Total_Number_Caught[RSDcat=="Stock"]),
              N.Quality=sum(Total_Number_Caught[RSDcat=="Quality"]),
              N.Preferred=sum(Total_Number_Caught[RSDcat=="Preferred"]),
              N.Memorable=sum(Total_Number_Caught[RSDcat=="Memorable"]),
              N.Trophy=sum(RSDcat=="Trophy"),
              PSD=round(sum(Total_Number_Caught[RSDcat%in%c("Quality","Preferred","Memorable","Trophy")])/
                          sum(Total_Number_Caught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.p=round(sum(Total_Number_Caught[RSDcat%in%c("Preferred","Memorable","Trophy")])/
                            sum(Total_Number_Caught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.m=round(sum(Total_Number_Caught[RSDcat%in%c("Memorable","Trophy")])/
                            sum(Total_Number_Caught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100),
              RSD.t=round(sum(Total_Number_Caught[RSDcat%in%c("Trophy")])/
                            sum(Total_Number_Caught[RSDcat%in%c("Stock","Quality","Preferred","Memorable","Trophy")])*100))%>%
    
    as.data.frame()
  
  
  message("Note that N.Stock means # of fish >=stock length but < Quality length. Total N.Stock = N.Stock+N.Quality+N.Preferred+N.Memorable+N.Trophy")
  return(RSDsum)
}