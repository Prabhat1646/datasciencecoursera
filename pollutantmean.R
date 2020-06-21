


pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
  
  lst <- lapply(fileNames, data.table::fread)
  dt <- rbindlist(lst)
  
  if (c(pollutant) %in% names(dt)){
    return(dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]])
  } 
}
