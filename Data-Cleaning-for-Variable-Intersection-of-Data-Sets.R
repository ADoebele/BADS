############################################################################
### Get data frames ########################################################
############################################################################

#DataCleaning_FeatureEngin on Known and Class Dataset have to run fist

# set working directory to path location 
#setwd("C:/Users/")

# Open the workspace "Orders_known"
load("Orders_known_updated.RData") 

# Open the workspace "Orders_class"
load("Orders_class_updated.RData") 

############################################################################
### Intersection of data frames ############################################
############################################################################

# give orders_class the variable return
orders_class$return = c(1:nrow(orders_class))

# get column names
names_class = names(orders_class)
names_known = names(orders_known)

# get intersection of names
names_intersection = intersect(names_class, names_known)

# delete unnecessary variables from orders_class
orders_class_inter = orders_class[, names_intersection]
names(orders_class_inter)

# delete unnecessary variables from orders_known (but keep return!)
orders_known_inter = orders_known[, names_intersection]
names(orders_known_inter)

############################################################################
### Clear workspace ########################################################
############################################################################

# remove vectors
rm(names_class)
rm(names_intersection)
rm(names_known)

############################################################################
### Save image from workspace ##############################################
############################################################################

# Save image
save.image(file="Orders_merged_updated.RData") 