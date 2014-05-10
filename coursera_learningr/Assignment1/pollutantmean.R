pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  output <- NULL
  if (length(id) > 0) {
    for ( cur_id in id ) {
      fn = sprintf("%s/%03d.csv",directory,cur_id)
      cur_data <- read.csv(fn)
      comp_data <- cur_data[complete.cases(cur_data),]
      
      output <- c(output, comp_data[,pollutant])
    }
  }
  
  return(mean(output))
}