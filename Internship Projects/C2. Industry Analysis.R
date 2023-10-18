
#Importing dataset
fin = read.csv( file.choose(), na.strings = c(""))
head(fin)
str(fin)
#------------------------------------------------------------------------------
# FACTORS
# categorical variable in R
fin$ID = factor(fin$ID)
fin$Name = factor(fin$Name)
fin$Industry = factor(fin$Industry)
fin$Inception = factor(fin$Inception)
fin$State = factor(fin$State)
fin$City = factor(fin$City)

#Factor Variable Trap
# while converting factor into numeric : direct conversion leads to wrong output
# therefore: factor > character > numeric
#------------------------------------------------------------------------------
#Replacing Value
#GSUB: all values & SUB:first value

fin$Expenses = gsub("Rupees","",fin$Expenses)
fin$Expenses = gsub(",","",fin$Expenses)
fin$Revenue = gsub("\\Rs.","",fin$Revenue)
fin$Revenue = gsub(",","",fin$Revenue)
fin$Growth = gsub("%", "",fin$Growth)

str(fin)

fin$Revenue = as.numeric(fin$Revenue)
fin$Expenses = as.numeric(fin$Expenses)
fin$Profit = as.numeric(fin$Profit)
fin$Growth = as.numeric(fin$Growth)


#------------------------------------------------------------------------------
#Dealing with MISSING DATA

sum(is.na(fin))
fin[!complete.cases(fin),] # to see NA containing rows


fin [which(fin$Revenue == 9746272), ] #which returns only those which have value = T
fin [is.na(fin$Inception),]

# fin_backup = fin

#------------------------------------------------------------------------------
# Removing records with NA

fin = fin [ !is.na(fin$Industry),]
fin

fin[!complete.cases(fin),]

# Resetting data frame index (after removing NA rows)

rownames(fin) = 1:nrow(fin) 


#------------------------------------------------------------------------------
# Replacing missing values: FACTUAL ANALYSIS [100% ACCURACY]

fin[is.na(fin$State),]

fin[is.na(fin$State) & fin$City == "New York", "State"] = "NY"

fin[c(11,377),]

fin[is.na(fin$State) & fin$City == "San Francisco", "State"] = "CA"

fin[c(82,265),]

#------------------------------------------------------------------------------
# Replacing missing values : MEDIAN IMPUTATION

##1
med_empl_retail = median(fin[fin$Industry == "Retail","Employees"],na.rm = T) #median of employees in retail industry as NA in employees is in Retail industry

fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] = med_empl_retail

fin[3,]

##2
med_empl_finserv =  median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = T)

fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] = med_empl_finserv

fin[330,]

fin[!complete.cases(fin),]

##3
med_growth_constr = median(fin[fin$Industry == "Construction", "Growth"], na.rm = T)

fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] = med_growth_constr

fin[8,]

##4
med_rev_constr = median(fin[fin$Industry == "Construction", "Revenue"], na.rm = T)

fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] = med_rev_constr

##5 
med_exp_constr = median(fin[fin$Industry == "Construction", "Expenses"], na.rm = T)

fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] = med_exp_constr

fin[is.na(fin$Profit),"Profit" ] = 
  fin[is.na(fin$Profit), "Revenue" ] - fin[ is.na(fin$Profit), "Expenses" ]
fin[c(8,42),]


##6
fin[is.na(fin$Expenses), "Expenses"] = 
  fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]
fin[15,]

#-------------------------------------------------------------------------------------------------------
#Visualising

library(ggplot2)

# Scatterplot showing revenue, expenses, profit classified by industry

p = ggplot(data = fin)

p + geom_point(aes(x = Revenue, y = Expenses , colour = Industry, size = Profit))

# Scatterplot showing industry trends for the expenses ~ revenue relationship 

d = ggplot(data = fin, aes(x = Revenue, y = Expenses , colour = Industry)) 

d + geom_point()+
  geom_smooth(fill=NA, size = 1.2)

# Boxplot showing growth by industry

f = ggplot(data = fin, aes(x = Industry, y = Growth, colour = Industry))

f + geom_jitter() +
  geom_boxplot(size = 1, alpha = 0.5, outlier.colour = NA)





