complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  output <- NULL
  if (length(id) > 0) {
    for ( cur_id in id ) {
      fn = sprintf("%s/%03d.csv",directory,cur_id)
      cur_data <- read.csv(fn)
      comp_data <- cur_data[complete.cases(cur_data),]
      
      output <- rbind(output, c(cur_id, dim(comp_data)[1]))
    }
  }
  colnames(output)<-c("id","nobs")
  return(data.frame(output))
}