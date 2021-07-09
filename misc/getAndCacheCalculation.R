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


