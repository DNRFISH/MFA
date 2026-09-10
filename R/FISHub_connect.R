#' Connect to the FISHub database
#'
#' Establishes a connection to the Michigan Fisheries database using Windows integrated authentication.
#'
#' @return A DBI database connection.
#'
#'
#' @export
#' 

FISHub_connect <- function() {
  
  DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "DNRSQLWEB",
    Database = "FISHReport",
    Trusted_Connection = "yes"
  )
  
}