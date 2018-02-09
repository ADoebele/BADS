# clear worspace
rm(list = ls())

################################################################################################
### Functions for Missing Data #################################################################
################################################################################################

# Count NAs per Column
count.NA.column <- function(x) {
  NAs=sapply(x, function(x) sum(is.na(x)))
  NAs=cbind.data.frame(Variables = names(NAs), NA.number = unname(NAs))  
  return(NAs)
}

# Count NAs per row 
count.NA.row <- function(x) {
  n=nrow(x)
  na_count <- apply(x, 1, function(x) sum(is.na(x)))
  obs=c(1:n)
  NAs=cbind.data.frame(Observation = obs, NA.number= na_count)
  return(NAs)
}

# Count NAs per row and add this information to data frame (from https://stackoverflow.com/questions/37801338/r-count-nas-per-row-in-dataframe)
add.NAs.per.row <- function(x) {
  x$NA_count <- apply(x, 1, function(x) sum(is.na(x)))
  return(x)
}

# NA percentage
NA.percentage <- function(x) {
  n=nrow(x)
  k=ncol(x)
  colNAs=count.NA.column(x) 
  sumNAs=sum(colNAs$NA.number)
  percentage=sumNAs/(n*k)
  return(percentage)
}