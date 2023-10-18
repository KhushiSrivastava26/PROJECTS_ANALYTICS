

dataset = read.csv(file.choose())
View(dataset)
dataset = dataset[,2:3]


# Fitting LINEAR REGRESSION MODEL
lin_reg = lm(formula = Salary~. , data = dataset)
summary(lin_reg)

# Fitting POLYNOMIAL REGRESSION MODEL
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary~. , data = dataset)

summary(poly_reg)


# Visualize linear regression model 

library(ggplot2)

ggplot()+
  geom_point(aes(x = dataset$Level , y= dataset$Salary), colour = "red")+
  geom_line( aes(x = dataset$Level , y= predict(lin_reg , newdata = dataset)),
             colour = "lightblue")+
  ggtitle("Truth or Bluff (Linear Regression)")+
  xlab("Level")+
  ylab("Salary")


# Visualize polynomial regression model 

x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot()+
  geom_point(aes(x = dataset$Level , y = dataset$Salary), colour = "green")+
  geom_line( aes(x = x_grid , y = predict(poly_reg, newdata = dataset)),
             colour = "blue")+
  ggtitle("Truth or Bluff [Polynomial Regression]")+
  xlab("Level")+
  ylab("Salary")

# predicting a new result with Linear regression
y_pred = predict(lin_reg , data.frame(Level = 6.5))

# predicting a new result with Polynomial regression
y_pred = predict(poly_reg , data.frame(Level = 6.5,
                                       Level2 = 6.5^2,
                                       Level3 = 6.5^3,
                                       Level4 = 6.5^4))

################################################################################

# SUPPORT VECTOR REGRESSION
#      EPSILON INSENSITIVE TUBE
#      SLACK VARIABLE Ei , Ei* [Below and above regression line respectively]


#Fitting SVR to dataset
library(e1071)
regressor = svm(formula = Salary ~ . ,
                data = dataset,
                type = "eps-regression")

y1_pred = predict(regressor, data.frame(Level = 6.5))

library(ggplot2)

ggplot()+
  geom_point(aes(x= dataset$Level , y = dataset$Salary),
             colour = "red")+
  geom_line(aes(x = dataset$Level , y = predict(regressor, newdata = dataset)),
            colour = "blue")+
  ggtitle("Truth or Bluff [SVR]")+
  xlab("Level")+
  ylab("Salary")

################################################################################

# Conclusion: The applicant's expected salary is justified at his level of experience.

################################################################################







