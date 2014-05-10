corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  output <- vector()

  for ( fn in dir(directory) ) {
    fn = sprintf("%s/%s",directory,fn)
    cur_data <- read.csv(fn)
    comp_data <- cur_data[complete.cases(cur_data),]
    
    if ( dim(comp_data)[1] >= threshold ) {
      c <- cor( comp_data[,"sulfate"], comp_data[,"nitrate"] )
      
      if(!is.na(c[1])) {
        output <- rbind(output, c[1])
      }
    } 
  }
    
  return(output)
}