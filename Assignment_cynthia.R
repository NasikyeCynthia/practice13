getwd()
#Checking if the continuous variables are normally distributed
install.packages("read.csv")
customerdata <- read.csv("Wholesalecustomersdata.csv")
print(customerdata)
boxplot(customerdata$Fresh)
boxplot(customerdata$Milk)
boxplot(customerdata$Grocery)
boxplot(customerdata$Frozen)
boxplot(customerdata$Detergents_Paper)
boxplot(customerdata$Delicassen)

#transforming the dataset to exclude any missing information
sum(is.na(customerdata))
customerdata_omit = na.omit(customerdata)
#There is no missing information

#Showing the outliers in only the continuous variables in dataset
outliers_fresh <- boxplot.stats(customerdata$Fresh)$out 
outliers_milk <- boxplot.stats(customerdata$Milk)$out
outliers_grocery <- boxplot.stats(customerdata$Grocery)$out
outliers_frozen <- boxplot.stats(customerdata$Frozen)$out
outliers_detergents <- boxplot.stats(customerdata$Detergents_Paper)$out
outliers_delicassen <- boxplot.stats(customerdata$Delicassen)$out

#Transforming the dataset and handling the outliers
IQR_fresh <- IQR(customerdata$Fresh)
IQR_Milk <- IQR(customerdata$Milk)
IQR_Grocery<- IQR(customerdata$Grocery)
IQR_Frozen <- IQR(customerdata$Frozen)
IQR_Detergents <- IQR(customerdata$Detergents_Paper)
IQR_Delicassen<- IQR(customerdata$Delicassen)

Q1_fresh <- 0.25*(sum(customerdata$Fresh))
Q1_milk <- 0.25*(sum(customerdata$Milk))
Q1_grocery <- 0.25*(sum(customerdata$Grocery))
Q1_frozen <- 0.25*(sum(customerdata$Frozen))
Q1_detergents <- 0.25*(sum(customerdata$Detergents_Paper))
Q1_delicassen <- 0.25*(sum(customerdata$Delicassen))

Q3_fresh <- 0.75*(sum(customerdata$Fresh))
Q3_milk <- 0.75*(sum(customerdata$Milk))
Q3_grocery <- 0.75*(sum(customerdata$Grocery))
Q3_frozen <- 0.75*(sum(customerdata$Frozen))
Q3_detergents <- 0.75*(sum(customerdata$Detergents_Paper))
Q3_delicassen <- 0.75*(sum(customerdata$Delicassen))

Tmin_fresh <- Q1_fresh - 1.5*IQR_fresh
Tmin_milk <- Q1_milk - 1.5*IQR_Milk
Tmin_grocery <- Q1_grocery - 1.5*IQR_Grocery
Tmin_frozen <- Q1_frozen - 1.5*IQR_Frozen
Tmin_detergents <- Q1_detergents - 1.5*IQR_Detergents
Tmin_delicassen <- Q1_delicassen - 1.5*IQR_Delicassen

Tmax_fresh <- Q3_fresh + 1.5*IQR_fresh
Tmax_milk <- Q3_milk + 1.5*IQR_Milk
Tmax_grocery <- Q3_grocery + 1.5*IQR_Grocery
Tmax_detergents <- Q3_detergents + 1.5*IQR_Detergents
Tmax_frozen <- Q3_frozen + 1.5*IQR_Frozen
Tmax_delicassen <- Q3_delicassen + 1.5*IQR_Delicassen

new_fresh <- customerdata$Fresh[which(customerdata$Fresh > Tmin_fresh & customerdata$Fresh < Tmax_fresh)]
new_milk <- customerdata$Milk[which(customerdata$Milk > Tmin_milk & customerdata$Milk < Tmax_milk)]
new_grocery <- customerdata$Grocery[which(customerdata$Frozen > Tmin_grocery & customerdata$Grocery < Tmax_grocery)]
new_frozen <- customerdata$Frozen[which(customerdata$Frozen > Tmin_frozen & customerdata$Frozen < Tmax_frozen)]
new_detergent <- customerdata$Detergents_Paper[which(customerdata$Detergents_Paper > Tmin_detergents & customerdata$Detergents_Paper < Tmax_detergents)]
new_delicassen <- customerdata$Delicassen[which(customerdata$Delicassen > Tmin_delicassen & customerdata$Delicassen < Tmax_delicassen)]

data_fresh = data.frame(new_fresh)
data_milk = data.frame(new_milk)
data_grocery = data.frame(new_grocery)
data_frozen = data.frame(new_frozen)
data_detergent = data.frame(new_detergent)
data_delicassen = data.frame(new_delicassen)

#Generates the highest annual spending
highest <- max(customerdata)
max(customerdata$Fresh)
max(customerdata$Milk)
max(customerdata$Grocery)
max(customerdata$Frozen)
max(customerdata$Detergents_Paper)
max(customerdata$Delicassen)
#Fresh has the highest annual spending pf 112151

#The retail channel customers use the most
getmode = function(v) {
  uniqv = unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]}
v = customerdata$Channel
mode_channel = getmode(customerdata$Channel)

#Region with lowest purchasing power
table(customerdata$Region)[[1]]
table(customerdata$Region)[[2]]
table(customerdata$Region)[[3]]
#Region 2 has the lowest purchasing power

#Region spends the most on milk
sum(customerdata$Milk[which(customerdata$Region=="1")])
sum(customerdata$Milk[which(customerdata$Region=="2")])
sum(customerdata$Milk[which(customerdata$Region=="3")])
#Region 3 spends the most on milk















