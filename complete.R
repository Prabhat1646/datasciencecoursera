complete <- function(directory,  id = 1:332) {
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
  
  lst <- lapply(fileNames, data.table::fread)
  dt <- rbindlist(lst)
  
  return(dt[complete.cases(dt), .(nobs = .N), by = ID])
  
}