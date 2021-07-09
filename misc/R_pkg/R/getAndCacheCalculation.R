#' @title getAndCacheCalculation
#' @description Query NASIS database by calculation name to store a flat-file (.txt) containing the CVIR code.
#' @param calcName The name of the calculation in NASIS.
#' @param fileName The name of the file to save the calculation in. default: \code{calcName}.txt.
#' @param basePath The path to a directory where calculation file will be stored.
#' @return Calculation CVIR code is written to specified file.
#' @author D.E. Beaudette
#' @examples 
#' getAndCacheCalculation(
#'    calcName = '15 AASHTO Group Index', 
#'    basePath = 'NASIS_calculations/'
#' )
#' @rdname getAndCacheCalculation
#' @export getAndCacheCalculation
getAndCacheCalculation <- function(calcName, fileName=paste0(calcName, '.txt'), basePath) {
  # init connection
  channel <- odbcDriverConnect(connection = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y365")
  
  # load single calculation by name
  qq <- sprintf(
    "
    SELECT *
    FROM calculation_View_0
    WHERE calc_nm = '%s'
    ;", 
    calcName
  )
  
  # get the calculation record, ignoring the text notes
  x <- sqlQuery(channel, qq, stringsAsFactors=FALSE)
  
  # format path to file
  fn <- file.path(basePath, fileName)
  cat(x$calc, file = fn)
}


