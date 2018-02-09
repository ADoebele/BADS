#############################################################################
##########################Main document######################################
#############################################################################


setwd("C:/Users/Tomas/Desktop/Buisness_Analytics/Assignment_Single")

orders = read.csv("BADS_WS1718_known.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)


##Load helper functions first

##Working on known dataset

# Check the data structure and the individual variables
str(orders)

# Check the summary statistics
summary(orders) 

# Unique value of character variables
str(unique(orders$order_date))
str(unique(orders$delivery_date))
str(unique(orders$item_size))
str(unique(orders$item_color))
str(unique(orders$user_title))
str(unique(orders$user_dob))
str(unique(orders$user_state))
str(unique(orders$user_reg_date))

# Level counts
table(unique(orders$order_date))
table(unique(orders$delivery_date))
table(unique(orders$item_size))
table(unique(orders$item_color))
table(unique(orders$user_title))
table(unique(orders$user_dob))
table(unique(orders$user_state))
table(unique(orders$user_reg_date))



#Import helper function to clean dataset and create new variables:

orders = CleanDataset(orders)

#get_again function to determin if a customer_id returned before:
orders$return = as.factor(orders$return)

again = get_again(orders)

orders$again = ifelse(orders$user_id %in% again$id, again$count, 0)



##Apply to unknown dataset:
orders_un = read.csv("BADS_WS1718_class.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)

#Clean data and create new variables
orders_un = CleanDataset(orders_un)

#Apply again dependent on user_id returned before
orders_un$again = ifelse(orders_un$user_id %in% again$id, again$count, 0)

