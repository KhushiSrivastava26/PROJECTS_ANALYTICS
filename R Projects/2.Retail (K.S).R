
library(tidymodels)
library(visdat)
library(tidyr)
library(ggplot2)
library(recipes)
library(dplyr)

#store_train = read.csv(file.choose())
#store_test = read.csv(file.choose()) # Future prediction data

# Visualising the dataset
vis_dat(store_train)

#
dim(store_train)

#-----------------------------------------------------------------------------------

# OUTLINE PRE-PROCESSING STEP FOR EACH DATA

#Id     :  numeric
#sales0 :  convert to numeric
#sales1 :  convert to numeric
#sales2 :  convert to numeric
#sales3 :  convert to numeric
#sales4 :  convert to numeric

#country : convert to character and create dummies: coded values for country 
#State : convert to character and create dummies: coded values for State
#storecode : get the string first 5, convert to factors and create dummies
#population : numeric
#store_Type : convert to factors and create dummies 
#countyname : drop it (can be grouped via country column)
#Areaname : drop it
#countytownname :drop it
#state_alpha : drop it (same as state)
#CouSub : drop it

#store : categorical 1/0 : target indicator var 1=opened 0=not opened 

length(unique(store_train$country)) #275
length(unique(store_train$State)) #54
length(unique(store_train$countyname)) #1491  
length(unique(store_train$storecode)) #1891 >> 2 
length(unique(store_train$Areaname)) #1891 
length(unique(store_train$countytownname)) #2372  
length(unique(store_train$state_alpha)) #54
length(unique(store_train$store_Type)) #4
length(unique(store_train$CouSub)) #1088


head(store_train)
#---------------------------------------------------------------------------------------

# Checking missing value with a median :

sum(is.na(store_train))
which(is.na(store_train$population))

#----------------------------------------------------------------------------------------

# Get the first 5 letters for store code :

fun_extract <- function(x){
  return(as.numeric(substr(x, 1, 5) == 'METRO'))
}

#---------------------------------------------------------------------------------------

# convert STATE & COUNTRY to character and then later convert them to dummies.

store_train$State <- as.character(store_train$State)
store_train$country <- as.character(store_train$country)

store_test$State <- as.character(store_test$State)
store_test$country <- as.character(store_test$country)

#---------------------------------------------------------------------------------------

dp_pipe=recipe(store ~ . , data = store_train)  %>% 
  update_role(Id, countyname, Areaname,countytownname,state_alpha,CouSub,
              new_role = "drop_vars") %>% 
  step_rm(has_role('drop_vars')) %>% 
  update_role(sales0,sales1,sales2,sales3,sales4,population,
              new_role = "to_numeric") %>%
  update_role(country,State,store_Type,
              new_role = "to_dummies") %>%
  update_role(storecode, new_role='convert') %>% 
  step_mutate_at(has_role('convert'), fn=fun_extract) %>% 
  step_mutate_at(has_role("to_numeric"),
                 fn=function(x)as.numeric(as.character(x))) %>% 
  step_novel(has_role('to_dummies') )%>% 
  # train and test data contains different levels of values for state and country;can lead to mismtch of data
  
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
  step_impute_median(all_numeric(),-all_outcomes())


print(dp_pipe)

dp_pipe = prep(dp_pipe)

train=bake(dp_pipe, new_data = NULL)
test=bake(dp_pipe,new_data= store_test)

head(data.frame(train))

#############################################################################################

# PREDICTIVE MODELLING


# SPLITTING DATA SET

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

# CLASSIFICATION MODEL

# install.packages('rpart')
library(rpart)

# Building the model on 80% data sample:
classifier = rpart(formula= store ~ . ,
                   data = train,
                   method = "class")

# Predicting the results of 20% data sample:
t2.pred = predict(classifier, newdata = t2, type = 'class')

# PREDICTING TEST SET RESULT

y_pred = predict(classifier , newdata = test, type = "class")

plot(y_pred, type = "l", main = " STORE PREDICTION",
     xlab = " Categories : Open = 1 | Not Open = 0",
     ylab = "No. of Stores") 

######

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier_1 = randomForest(store ~ .,
                            data = t1,
                            ntree = 100)

# Predicting the Test set results
t2.pred1 = predict(classifier_1, newdata = t2)

plot(classifier)
plot(classifier_1)


# GENERATING OUTPUT FOR SUBMISSION

write.csv(y_pred,"Retail.csv",row.names = F)

# 
varImpPlot(classifier_1, main = "Variable Importance Plot")

#--------------------------------------------------------------------------------

######################################################################################

# EXTRACTING INSIGHTS FROM DATA

#--------------------------------------------------------------------------------------

### What is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME? ###


# Filter for Supermarket Type1 in Kennebec County, ME
filtered_data <- store_train %>%
  filter(store_Type == "Supermarket Type1" & countyname == "Kennebec County" & state_alpha == "ME")

# Calculate the total sales
total_sales <- sum(filtered_data$sales0 + filtered_data$sales1 + filtered_data$sales2 + filtered_data$sales3 + filtered_data$sales4)

# Print the total sales
cat("Total Sales in Supermarket Type1 in Kennebec County, ME:", total_sales, "\n")

#----------------------------------------------------------------------------------------


### What is the response rate for Grocery Stores? ###

# Filter for grocery stores
grocery_store_data <- store_train %>%
  filter(store_Type == "Grocery Store")

# Calculate the response rate
response_rate <- mean(grocery_store_data$store) * 100  # Multiply by 100 to convert to percentage

# Round the response rate to two decimal places
response_rate <- round(response_rate, 2)

# Print the response rate
cat("Response Rate for Grocery Stores:", response_rate, "%\n")

#----------------------------------------------------------------------------------------

### Does the sales column follow a normal distribution? ###

# Visual Inspection (Histogram and Q-Q Plot)
par(mfrow = c(1, 2))  # Create a 1x2 grid of plots
hist(store_train$sales0, main = "Histogram of Sales0", xlab = "Sales0")
qqnorm(store_train$sales0, main = "Q-Q Plot of Sales0")
qqline(store_train$sales0)

# Statistical Test (Shapiro-Wilk)
shapiro_test <- shapiro.test(store_train$sales0)

# Print the results of the test
cat("Shapiro-Wilk Test for Sales0:\n")
cat("p-value:", shapiro_test$p.value, "\n")

# Check the p-value to determine normality
if (shapiro_test$p.value < 0.05) {
  cat("The data does not follow a normal distribution.\n")
} else {
  cat("The data follows a normal distribution.\n")
}

###
# Visual Inspection (Histogram and Q-Q Plot)
par(mfrow = c(1, 2))  # Create a 1x2 grid of plots
hist(store_train$sales1, main = "Histogram of Sales0", xlab = "Sales1")
qqnorm(store_train$sales1, main = "Q-Q Plot of Sales0")
qqline(store_train$sales1)

# Statistical Test (Shapiro-Wilk)
shapiro_test <- shapiro.test(store_train$sales0)

# Print the results of the test
cat("Shapiro-Wilk Test for Sales1:\n")
cat("p-value:", shapiro_test$p.value, "\n")

# Check the p-value to determine normality
if (shapiro_test$p.value < 0.05) {
  cat("The data does not follow a normal distribution.\n")
} else {
  cat("The data follows a normal distribution.\n")
}

# Answer : NO 

#----------------------------------------------------------------------------------------


### What are the number of outliers for Total Sales? ###

# Calculate the IQR (Interquartile Range) of total sales
q1 <- quantile(store_train$sales0, 0.25)
q3 <- quantile(store_train$sales0, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper limits for outliers
lower_limit <- q1 - 1.5 * iqr
upper_limit <- q3 + 1.5 * iqr

# Identify outliers
outliers <- store_train$sales0 < lower_limit | store_train$sales0 > upper_limit

# Count the number of outliers
num_outliers <- sum(outliers)

cat("Number of Outliers for Total Sales:", num_outliers, "\n")

#----------------------------------------------------------------------------------------

### Which Store type has the maximum variance in Total Sales? ###

# Calculate the total sales for each store
store_train$total_sales <- rowSums(store_train[, c("sales0", "sales1", "sales2", "sales3", "sales4")])

# Calculate the variance of total sales for each store type
variance_by_store_type <- store_train %>%
  group_by(store_Type) %>%
  summarise(variance_total_sales = var(total_sales))

# Find the store type with the maximum variance
max_variance_store_type <- variance_by_store_type %>%
  filter(variance_total_sales == max(variance_total_sales))

# Print the store type with maximum variance
cat("Store Type with Maximum Variance in Total Sales:", max_variance_store_type$store_Type, "\n")

#----------------------------------------------------------------------------------------
























