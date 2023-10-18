
library(tidymodels)
library(visdat)
library(tidyr)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)


housing_train = read.csv(file.choose())
housing_test = read.csv(file.choose()) # Future prediction data

# Visualising the dataset
vis_dat(housing_train)

# Outline data pre-processing steps for each variable:
#Suburb : categorical :convert to numeric and create dummies 
#Address : categorical :convert to factor and create dummies
#Rooms : numeric 
#Type : categorical :convert to numeric and create dummies
#Method : categorical :convert to numeric and create dummies 
#SellerG : categorical :
#Distance : numeric 
#Postcode : categorical : drop it
#Bedroom2 : Numeric 
#Bathroom : numeric 
#Car : numeric 
#Landsize : numeric 
#BuildingArea : numeric 
#YearBuilt : take year difference   
#CouncilArea : convert to numeric and create dummies



#unique(housing_train$Type)        #3
#unique(housing_train$SellerG)     #182
#unique(housing_train$Method)      #5
#unique(housing_train$CouncilArea) #20
#unique(housing_train$Suburb)      #142
#unique(housing_train$Address)

#----------------------------------------------------------------------------------------

# DATA PRE-PROCESSING

# define our "dependent/ target var" and "independent vars"
# assign new roles
# create dummy variables
# fill missing data
#____________________________________________________________
## Function for Address

Address_func=function(x){
  x=stringr::str_extract(x, "\\w+$")
  return(x)
}

#__________________________________________________________
## Function for YearBuilt

property_age <- function(year_built) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))  # Get the current year
  age <- current_year - year_built
  return(age)
}

#__________________________________________________________

dp_var = recipe(Price ~ . , data = housing_train) %>%
  update_role(Postcode, new_role = 'dropvars') %>% 
  update_role(Rooms,Bedroom2,Bathroom, Car, Landsize, BuildingArea, 
              new_role = "to_numeric") %>%
  update_role(Suburb, SellerG, CouncilArea, Type, Method,
              new_role = "to_factor") %>% 
  update_role(Suburb, SellerG, CouncilArea, Type, Method, new_role = "to_dummies") %>% 
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>%
  step_rm(has_role('dropvars')) %>% 
  step_mutate_at(has_role("to_numeric"), fn = as.numeric) %>%
  step_mutate_at(has_role( "to_factor"), fn = as.factor) %>%
  step_mutate_at(Address, fn=Address_func) %>% 
  step_mutate_at(YearBuilt, fn=property_age) %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>%
  step_impute_median(all_numeric(),-all_outcomes())


print(dp_var)

dp_var = prep(dp_var)

train=bake(dp_var, new_data = NULL)
test=bake(dp_var,new_data=housing_test)


# Check missing values
vis_dat(train) # Processed data
vis_dat(housing_train) # Raw data

#---------------------------------------------------------------------------------------

# PREDICTIVE MODELLING


# SPLITTING DATA SET

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

#------------------------------------------------------------------------------------------------

# FITTING LINEAR MODEL TO TRAINING DATA SET

#1
regressor = lm(formula = Price ~ ., data = t1)

summary(regressor)

#2
regressor = lm(formula = Price ~ . -CouncilArea_Glen.Eira -SellerG_Ray -SellerG_Nelson
               -SellerG_hockingstuart -SellerG_Brad -SellerG_Biggin -Suburb_X__other__
               -Suburb_Richmond, data = t1)

summary(regressor)

#3
regressor = lm(formula = Price ~ . -CouncilArea_Glen.Eira -SellerG_Ray -SellerG_Nelson
               -SellerG_hockingstuart -SellerG_Brad -SellerG_Biggin -Suburb_X__other__
               -Suburb_Richmond -Method_X__other__ -SellerG_Woodards -CouncilArea_Port.Phillip
               -Method_VB -Method_SP -CouncilArea_X__other__,
               data = t1)

summary(regressor)

fit=stats::step(regressor)
formula(fit)
#############################################################################################

# FITTING RANDOM FOREST MODEL TO TRAINING DATA SET

model_rf <- randomForest(
  Price ~ .,
  data = train,
  ntree = 100,     # Number of trees in the forest
  mtry = sqrt(ncol(train)),   # Number of variables tried at each split
  importance = TRUE  # Calculate variable importance
)

# Predict on the t2 (0.2) sample set
# predictions <- predict(model_rf, newdata = test)

t2.pred = predict(model_rf , newdata=t2)


##
errors=t2$Price-t2.pred

rmse=errors**2 %>% mean() %>% sqrt()

Score = 212467/rmse
# 0.847



# PREDICTING TEST SET RESULT

y_pred = predict(model_rf , newdata = test)

plot(y_pred, type = "l") 


# VISUALISE

library(ggplot2)

# Create a sequence of values for x_grid
x_grid <- seq(min(train$Price), max(train$Price), length.out = 1885)


# Create a data frame for plotting
plot_data <- data.frame(Price = x_grid, Predicted = y_pred)

# Create the scatter plot with the regression line
ggplot(plot_data, aes(x = Price, y = Predicted)) +
  geom_point(color = "green") +
  geom_line(color = "blue") +
  geom_smooth(color = "black", method = "lm", se = FALSE)+
  ggtitle("Random Forest Regression") +
  xlab("Price") +
  ylab("Predicted Price")

library(ggplot2)

# Create aggregated data
aggregated_data <- aggregate(Predicted ~ Price, data = plot_data, FUN = mean)

# Create the scatter plot with aggregated line
ggplot(aggregated_data, aes(x = Price, y = Predicted)) +
  geom_point(color = "red") +
  geom_smooth(color = "blue", method = "lm", se = FALSE) +
  ggtitle("Random Forest Regression (Aggregated)") +
  xlab("Price") +
  ylab("Predicted Price")


### finding cutoff for hard classes

train.score=train_pred$.pred_1

real=train$Revenue.Grid

# KS plot

rocit = ROCit::rocit(score = train.score, 
                     class = real) 

kplot=ROCit::ksplot(rocit)

# GENERATING OUTPUT 

write.csv(y_pred,"Khushi_Srivastava_RealEstate.csv",row.names = F)

######################################################################################

# EXTRACTING INSIGHTS FROM DATA

#-------------------------------------------------------------------------------------------------


### What is the difference in average price between house type h and t? ###

# Calculate the average price for house type 'h'
avg_price_h = mean(housing_train$Price[housing_train$Type == 'h'], na.rm = TRUE)

# Calculate the average price for house type 't'
avg_price_t <- mean(housing_train$Price[housing_train$Type == 't'], na.rm = TRUE)

# Calculate the difference in average price
price_difference <- avg_price_h - avg_price_t

# Print the result
print(paste("Difference in average price between house types h and t:", price_difference))

#-------------------------------------------------------------------------------------------------

# Unique values of postcode

length(unique(housing_train$Postcode))

# Distance : Normal distribution or not?

# Visualize the distribution with a histogram
hist(housing_train$Distance, main = "Histogram of Distance")

# Perform the Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(housing_train$Distance)

# Print the test result
print(shapiro_test)

#-------------------------------------------------------------------------------------------------


# Which seller has maximum value transactions? ( Sum of Price)

# Calculate the sum of Price for each seller
seller_transactions <- aggregate(Price ~ SellerG, data = housing_train, sum)

# Find the seller with the maximum sum of Price
max_seller <- seller_transactions[which.max(seller_transactions$Price), ]

# Print the result
print(paste("Seller with maximum value transactions:", max_seller$SellerG))

#-------------------------------------------------------------------------------------------------


#### Which CouncilArea has maximum average price? ###

# Calculate the average price for each CouncilArea
councilarea_avg_price <- aggregate(Price ~ CouncilArea, data = housing_train, mean)

# Find the CouncilArea with the maximum average price
max_avg_councilarea <- councilarea_avg_price[which.max(councilarea_avg_price$Price), ]

# Print the result
print(paste("CouncilArea with maximum average price:", max_avg_councilarea$CouncilArea))

#-------------------------------------------------------------------------------------------------


### Which CouncilArea has maximum variance in the price? ###

# Calculate the variance of Price for each CouncilArea
councilarea_variance_price <- aggregate(Price ~ CouncilArea, data = housing_train, var)

# Find the CouncilArea with the maximum variance in Price
max_variance_councilarea <- councilarea_variance_price[which.max(councilarea_variance_price$Price), ]

# Print the result
print(paste("CouncilArea with maximum variance in price:", max_variance_councilarea$CouncilArea))

#-------------------------------------------------------------------------------------------------


### Find the variance of the target variable 'Price'. ###

# Calculate the variance of the 'Price' variable
price_variance <- var(housing_train$Price, na.rm = TRUE)

# Print the calculated variance
print(paste("Variance of Price:", price_variance))

#-------------------------------------------------------------------------------------------------

### Find out how many observations have missing values for variable 'YearBuilt'? ###

# Count the number of missing values for 'YearBuilt'
missing_yearbuilt <- sum(is.na(housing_train$YearBuilt))

# Print the result
print(paste("Number of observations with missing values for YearBuilt:", missing_yearbuilt))






