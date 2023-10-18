
# Importing dataset

dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
View(dataset)


# APRIORI

# Data Pre-Processing
library('arules')
#  creating SPARK MATRIX
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T) 
summary(dataset)

itemFrequencyPlot(dataset, topN = 100) #to determine the support

# Training Apriori on the dataset

#  Setting min support and confidence [based on business circumstances]
#     to find value of support :
#     3*7/7500 = 0.0028 ~ 0.003 [we set min 3 times product is bought everyday / total no of transactions]
#     to find value of confidence : default is 0.8
rules = apriori(data = dataset, parameter = list(support = 0.003, confidence = 0.2)) 

# Visualising the result
inspect(sort(rules , by = 'lift')[1:10])

###############################################################################################################

# ECLAT 
#  association finding rule
#  uses set
#  Only Support ; in form of set [eg. set of items]

# Data Pre-Processing
library('arules')
#  creating SPARK MATRIX
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T) 
summary(dataset)

itemFrequencyPlot(dataset, topN = 100) #to determine the support

# Training Eclat on the dataset

#  Setting min support [based on business circumstances]
#     to find value of support :
#     3*7/7500 = 0.0028 ~ 0.003 [we set min 3 times product is bought everyday / total no of transactions]
#     minlen we want sets of atleast 2 items purchased together
rules = eclat(data = dataset, parameter = list(support = 0.003, minlen = 2))

# Visualising the result
inspect(sort(rules , by = 'support')[1:10])





