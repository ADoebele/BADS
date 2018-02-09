#####################################################################################
###########################Helper Functions##########################################
#####################################################################################



#Function to combine rare values
MergeRareValues = function(vector, threshold = 100, newName="rare") {
  toMerge = names(which(table(vector) < threshold))
  vector = ifelse(vector %in% toMerge, newName, vector)
  print(table(vector))
  return(vector)
}


#Function to clean dataset and create new variables
CleanDataset = function(data){
  if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
  if(!require("caret")) install.packages("caret"); library("caret")
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
  if(!require("eeptools")) install.packages("eeptools"); library("eeptools")
  if(!require("zoo")) install.packages("zoo"); library("zoo")
  if(!require("data.table")) install.packages("data.table"); library("data.table")

  ############################################################################
  ### Recombine Variables ####################################################
  ############################################################################  
  
  #1. item_color
  #We use the defined function MergeRareValues to combine rare colors
  
  data$item_color <- MergeRareValues(data$item_color)
  
  #Would be interesting to combine colours depending on their colour spectrum. 
  
  
  
  #2. item_size
  #In the first step we drop variables that are below our treshold of 40:
  data$item_size <- MergeRareValues(data$item_size, newName = "rare size")
  #We have to take care of different items, e.g(Clothing,Shoes). First we focus on clothing:
  #In order_size we see the same values in different writing, e.g. "l" and "L". Therefore we need to combine those values.
  data$item_size <- ifelse(data$item_size %in% c("l","xl","xxl","m","s","xs","xxxl"),toupper(data$item_size),data$item_size)
  #Smaller differences in sizes like 39 and 39+ will be combined as 39
  data$item_size <- ifelse(data$item_size %in% c("3+","4+","5+","6+","7+","8+","9+","10+","11+","12+","37+","38+","39+","40+","41+","42+"),gsub('.{1}$', '', data$item_size),data$item_size)
  
  
  #3. order_date
  
  #The class of order_date is character, we want to work with dates:
  print(data$order_date)
  print(ymd(data$order_date))
   data$order_date = ymd(data$order_date)
  
  
  #4. delivery_date
  
  #We replace the values of ? by NA
  data$delivery_date = ifelse(data$delivery_date == "?",NA,data$delivery_date)
  #Class of delivery_date as "date"
  data$delivery_date= ymd(data$delivery_date)
  #We replace the NA values by the order_date + the mean in the differences between order and delivery date(require(lubridate))
  data$delivery_date[is.na(data$delivery_date)] =
    data$order_date[is.na(data$delivery_date)] +
    mean(difftime(data$order_date,data$delivery_date,units="days"),na.rm=TRUE)
  
  
  #5. user_title
  # We are mainly interested on the difference between men and women in our dataset.Therefore we replace Company,Family and not reported as "other"
  # One might add, that the amount of male observations in our dataset is only 3,5%
  data$user_title <- ifelse(data$user_title %in% c("Family","Company","not reported"), "other" ,data$user_title)
  
  ##6. user_dob
  u_user_dob = unique(data$user_dob)
  #Replacing missing Values
  data$user_dob = ifelse(data$user_dob == "?",NA,data$user_dob)
  #Date format
  data$user_dob = ymd(data$user_dob)
  #Calculate numeric age from date of birth by function age
  age = function(dob, current) {
    dob = as.POSIXlt(dob)
    current = as.POSIXlt(current)
    age = current$year - dob$year
    ifelse(current$mon < dob$mon | (current$mon == dob$mon & current$mday < dob$mday),
           age - 1, age)
  }
  data$customer_age = age(data$user_dob,data$order_date)
  data$cumstomer_age_num = data$customer_age
  
  ############################################################################
  ### Create new Variables ###################################################
  ############################################################################
  
  #Calculate groups of age
  
  data$customer_age = ifelse(data$customer_age <= 18,18,data$customer_age)
  data$customer_age = ifelse(data$customer_age > 18 & data$customer_age <= 25, 25 ,data$customer_age)
  data$customer_age = ifelse(data$customer_age > 25 & data$customer_age <= 35, 35 ,data$customer_age)
  data$customer_age = ifelse(data$customer_age > 35 & data$customer_age <= 50, 50 ,data$customer_age)
  data$customer_age = ifelse(data$customer_age > 50 & data$customer_age <= 70, 70 ,data$customer_age)
  data$customer_age = ifelse(data$customer_age > 70 , 100 ,data$customer_age)
  

  #Add East-West variable
  data$east_west = ifelse(data$user_state %in% c("Saxony", "Lower Saxony", "Thuringia", "Berlin", "Brandenburg","Mecklenburg-Western Pomerania"), "east","west")
  
  #Days passed since delivery:
  data$days_sd = data$delivery_date - data$order_date
  data$days_sd_num = data$day_sd
  #Group into one week, two weeks and three weeks
  
  data$days_sd[data$days_sd <= 3] = 3
  data$days_sd[data$days_sd > 3 & data$days_sd <= 7] = 7
  data$days_sd[data$days_sd <= 14 & data$days_sd > 7] = 14
  data$days_sd[data$days_sd > 14 ] = 21
  
  #basket of goods value:
  #Create subset with relevant variables:
  data_sub = select(data, user_id, item_price, order_date) #library(dplyr)
  basket_value = as.data.table(data_sub)[, sum(item_price), by = .(order_date, user_id)]
  
  
  #value of each order (shopping cart size)
  cart_size_per_day = data %>%
    group_by(order_date, user_id) %>%
    summarize(cart_size = sum(item_price))
  
  data = merge(data, cart_size_per_day, by=c("order_date", "user_id"))
  
  #delete na-column cart_size_per_day when existing
  del = colnames(data)!="cart_size_per_day"
  data = data[del]
  
  #Buying volume of each customer
  vol = data %>%
    group_by(user_id) %>%
    summarize(Volume = sum(item_price))
  
  data = merge(data, vol, by=c("user_id"))
  
  #sales items
  sales = data %>%
    group_by(item_id) %>%
    summarize(max_price = max(item_price))
  
  data = merge(data, sales, by="item_id")
  
  data$sale <- data$item_price != data$max_price
  
  #delete column max_price
  del = colnames(data)!="max_price"
  data = data[del]
   
}


#Did a certain customer return before?
get_again = function(data){
  ids = data$user_id[data$return == 1]
  again = aggregate(data.frame(count = ids), list(id = ids), length)  
}

