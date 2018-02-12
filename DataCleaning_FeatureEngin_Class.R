############################################################################
### Preparation ############################################################
############################################################################

# load packages
library(eeptools)
require(zoo)        
require(lubridate)
require(dplyr)
library(data.table)

# set working directory to path location
#setwd("C:/Users/")

# read data frame
orders = read.csv(file="BADS_WS1718_class_20180115.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#load helperfunctions
source("helperfuncs.r")

############################################################################
### Data Cleaning and Feature Engineering ##################################
############################################################################

# clean data set
orders = CleanDataset(orders)


#Check how many times user has returned before
get_again = function(){
  tmp = read.csv(file="BADS_WS1718_known.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
  ids = tmp$user_id[tmp$return==1]
  again = aggregate(data.frame(count = ids), list(id = ids), length)
}

again = get_again()
orders$again = ifelse(orders$user_id %in% again$id, again$count,0)

############################################################################
### Factorizing Variables ##################################################
############################################################################

orders$item_id = as.factor(orders$item_id)
orders$user_id = as.factor(orders$user_id)
orders$order_date = as.factor(orders$order_date)
orders$order_item_id = as.factor(orders$order_item_id)
orders$delivery_date = as.factor(orders$delivery_date)
orders$item_size = as.factor(orders$item_size)
orders$item_color = as.factor(orders$item_color)
orders$brand_id = as.factor(orders$brand_id)
orders$user_title = as.factor(orders$user_title)
orders$user_dob = as.factor(orders$user_dob)
orders$user_state = as.factor(orders$user_state)
orders$user_reg_date = as.factor(orders$user_reg_date)
orders$customer_age = as.factor(orders$customer_age)
orders$east_west = as.factor(orders$east_west)
orders$sale= as.factor(orders$sale)

############################################################################
### Create Dummy Variables of high dimensional variables ###################
############################################################################

# dichotomize item_size and clear the names of the variables
item_size_bin = cbind(with(orders, model.matrix(~ orders$item_size + 0)))
item_size_bin = as.data.frame(item_size_bin)
item_size_bin = item_size_bin %>% mutate_if(is.numeric,as.factor)
names = names(item_size_bin) # change names of the variables
names(item_size_bin) = substring(names, 8)
orders = cbind(orders, item_size_bin)
rm(item_size_bin)

# dichotomize item_color
item_color_bin = cbind(with(orders, model.matrix(~ orders$item_color + 0)))
item_color_bin = as.data.frame(item_color_bin)
item_color_bin = item_color_bin %>% mutate_if(is.numeric,as.factor)
names = names(item_color_bin) # change names of the variables
names(item_color_bin) = substring(names, 8)
orders = cbind(orders, item_color_bin)
rm(item_color_bin)

# dichotomize brand_id
brand_id_bin = cbind(with(orders, model.matrix(~ orders$brand_id + 0)))
brand_id_bin = as.data.frame(brand_id_bin)
brand_id_bin = brand_id_bin %>% mutate_if(is.numeric,as.factor)
names = names(brand_id_bin) # change names of the variables
names(brand_id_bin) = substring(names, 8)
orders = cbind(orders, brand_id_bin)
rm(brand_id_bin)

# change column names
colnames(orders)[which(names(orders) == "item_size2+")] <- "item_size2plus"  
colnames(orders)[which(names(orders) == "item_size36+")] <- "item_size36plus"  
colnames(orders)[which(names(orders) == "item_size43+")] <- "item_size43plus"  
colnames(orders)[which(names(orders) == "item_size44+")] <- "item_size44plus"  
colnames(orders)[which(names(orders) == "item_size45+")] <- "item_size45plus"  
colnames(orders)[which(names(orders) == "item_size46+")] <- "item_size46plus"  
colnames(orders)[which(names(orders) == "item_colordark denim")] <- "item_colordark_denim"  
colnames(orders)[which(names(orders) == "item_color?")] <- "item_color_questionmark"  
colnames(orders)[which(names(orders) == "item_colorantique pink")] <- "item_colorantique_pink"  
colnames(orders)[which(names(orders) == "item_colorbaltic blue")] <- "item_colorbaltic_blue"  
colnames(orders)[which(names(orders) == "item_colorcobalt blue")] <- "item_colorcobalt_blue"  
colnames(orders)[which(names(orders) == "item_colorcopper coin")] <- "item_colorcopper_coin"  
colnames(orders)[which(names(orders) == "item_colorcurrant purple")] <- "item_colorcurrant_purple"  
colnames(orders)[which(names(orders) == "item_colordark garnet")] <- "item_colordark_garnet"  
colnames(orders)[which(names(orders) == "item_colordark grey")] <- "item_colordark_grey"  
colnames(orders)[which(names(orders) == "item_colordark navy")] <- "item_colordark_navy"  
colnames(orders)[which(names(orders) == "item_colordark oliv")] <- "item_colordark_oliv"  
colnames(orders)[which(names(orders) == "item_sizerare size")] <- "item_sizerare_size"  

############################################################################
### NA analysis and imputation #############################################
############################################################################

# Count NAs per Column
count.NA.column <- function(x) {
  NAs=sapply(x, function(x) sum(is.na(x)))
  NAs=cbind.data.frame(Variables = names(NAs), NA.number = unname(NAs))  
  return(NAs)
}

# find out which variables have NAs
count.NA.column(orders)

# load package
require(Hmisc)

# impute NAs and delete NAs
orders$customer_age_num = impute(orders$cumstomer_age_num)
orders$customer_age = impute(orders$customer_age)
orders$cumstomer_age_num = NULL

# check whether variable selection was successful
count.NA.column(orders)

# transform days_sd
orders$days_sd = as.numeric(orders$days_sd)

############################################################################
### Drop unrelevant Variables ##############################################
############################################################################

# save data set with all variables
full_data_set_class = orders

# remove the old factor levels from orders
orders$item_size = NULL
orders$item_color = NULL
orders$brand_id = NULL

# remove variables which are not relevant for the analysis
orders$user_id = NULL
orders$item_id = NULL
orders$order_date = NULL
orders$user_dob = NULL
orders$user_reg_date = NULL
orders$delivery_date = NULL

# rename "orders" to "orders_class"
orders_class = orders

############################################################################
### Clean working memory ###################################################
############################################################################

# remove dataframes
rm(again)
rm(orders)

# remove functions
rm(CleanDataset)
rm(count.NA.column)
rm(get_again)
rm(MergeRareValues)

# remove values
rm(names)

############################################################################
### Safe data frame ########################################################
############################################################################

# Save cleaned workspace with data frames
save.image(file="Orders_class_updated.RData")