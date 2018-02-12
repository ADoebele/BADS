############################################################################################
############### Descriptive table (with counting NAs) ######################################
############################################################################################

descriptive.table <- function (x) {
  ncol.x=ncol(x)
  matrix.x=as.data.frame(matrix(ncol = 6,nrow = ncol.x))
  rownames(matrix.x)=colnames(x)
  colnames(matrix.x)=c("Q1","Q2","Mean","Q3","IQR","NAs")
  
  for (i in 1:ncol.x) {
    
    matrix.x[i,1]=quantile(x[,i],probs = 0.25)
    matrix.x[i,2]=quantile(x[,i],probs = 0.50)
    matrix.x[i,3]=mean(x[,i])
    matrix.x[i,4]=quantile(x[,i],probs = 0.75)
    matrix.x[i,5]=matrix.x[i,4]-matrix.x[i,1]
    matrix.x[i,6]=sum(round(is.na(data[,i])))
  }
  
  matrix.x=round(matrix.x,digits=2)
  return(matrix.x)
  
}    

# load relevant package
library(xtable)

# create the descriptive table
d.table=descriptive.table(data)    
xtable(d.table)

#########################################################################
##### Create histogram function #########################################
#########################################################################

# data is the data frame 
# variable is the variable name and MUST be a string
# bandwith is the bandwith

histogram <- function (data, variable, bandwith) {

  ggplot(data, aes(x=eval(parse(text = variable)))) + 
    geom_histogram(aes(y=..density..), binwidth=bandwith, colour="white", fill="#0072B2") +
    geom_density(alpha=.3, colour="#56B4E9", fill="#56B4E9") +
    xlab(variable) +
    ylab("frequency") +
    coord_cartesian(ylim = c(0,1))

}

# Histogram for Customer_age
histogram(orders, variable = "item_price", bandwith=100)