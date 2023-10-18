
getwd()
setwd("C://Users//ksriv//Documents//MBA//2. R//Udemy.R" )
movies = read.csv("P2-Movie-Ratings.csv")
head(movies)
colnames(movies) = c("Film", "Genre", "CriticRating","AudienceRating", "BudgetMillions","Year")
head(movies)
str(movies)
summary(movies)

# Converting into factor

movies$Year = factor(movies$Year)
movies$Film = factor(movies$Film)
movies$Genre = factor(movies$Genre)

#-------------------------------------------------------------------------------

# AESTHETICS
library(ggplot2)

ggplot(data = movies, aes(x = CriticRating , y = AudienceRating))

# add geometry layer
ggplot(data = movies, aes(x = CriticRating , y = AudienceRating))+
  geom_point()

# add color
ggplot(data = movies, aes(x = CriticRating , y = AudienceRating , 
                          colour = Genre)) + 
  geom_point()

# add size 
ggplot(data = movies, aes(x = CriticRating , y = AudienceRating , 
                          colour = Genre , size = BudgetMillions)) + 
  geom_point()


# Plotting With Layers

p = ggplot(data = movies, aes(x = CriticRating , y = AudienceRating , 
                              colour = Genre ,  size = BudgetMillions)) 

p + geom_line() + geom_point()

# Overriding aesthetics

p + geom_point(aes(size = CriticRating))

p + geom_point(aes(size = BudgetMillions))

p + geom_point(aes(x = BudgetMillions))
#   we can change the x, y axis as well however in graph the names of axis do 
#   not change. So to do so : 

p + geom_point(aes(x = BudgetMillions)) +
  xlab("BudgetMillions")

# IN ALL THE ABOVE OVERRIDING OUR P REMAINS THE SAME & IS NOT CHANGING

p + geom_point( size = 3)


# MAPPING vs SETTING

r = ggplot(data = movies , aes(x = CriticRating, y = AudienceRating))

#Mapping: 
r + geom_point(aes(colour = Genre))

#Setting
r + geom_point(colour = "DarkGreen")

#ERROR : r+ geom_point(aes(colour = "DarkGreen"))


#-------------------------------------------------------------------------------

# HISTOGRAM , BARCHARTS

s = ggplot(data = movies , aes(x = BudgetMillions))
s + geom_histogram(binwidth = 10)

# statistics layer as count variable on y axis is  generated, grouped by bin 

s+ geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black")


s + geom_density(aes(fill = Genre), position = "stack")




#-------------------------------------------------------------------------------
# LAYERING TIPS

#Method 1 :

t = ggplot(data = movies, aes (x = AudienceRating))
t + geom_histogram(binwidth = 10, fill = "White", colour = "Black")

#Method 2 :

t = ggplot(data = movies)
t + geom_histogram(binwidth = 10,
                   aes(x = AudienceRating), 
                   fill = "White", colour = "Black")

# when you want extra flexibility, when what is required is unknown use M2

t + geom_histogram(binwidth = 10,
                   aes(x = CriticRating), 
                   fill = "White", colour = "Black")

#-------------------------------------------------------------------------------
# STATISTICAL TRANSFORMATION

u = ggplot(data = movies, aes(x =  CriticRating, y = AudienceRating, 
                              colour = Genre))
u + geom_point() + geom_smooth(fill = NA)


#Boxplots

g = ggplot(data = movies, aes(x = Genre, y = AudienceRating, 
                              colour = Genre))
g + geom_boxplot(size = 1.2) + geom_point()

g + geom_boxplot(size = 1.2) + geom_jitter()

g + geom_jitter()+ geom_boxplot(size = 1.2 ,  alpha = 0.5)
#alpha = transparency

#-------------------------------------------------------------------------------
# FACETS

v = ggplot(data = movies, aes(x =  BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black")

# not clear therefore we will use facet:

v + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black") +
  facet_grid(Genre~., scales="free")   

# [ (RowName ~ Colname) ,  scales=free allows each to have its own axis ]

# SCATTERPLOTS
u + geom_point(size = 3) +
  facet_grid(Genre~.)

u + geom_point(size = 3) +
  facet_grid(.~Year)

u + geom_point(aes(size = BudgetMillions)) +
  geom_smooth()+
  facet_grid(Genre~Year)


#-------------------------------------------------------------------------------
# COORDINATES

m = ggplot(data = movies , aes(x = CriticRating, y = AudienceRating,
                               size =  BudgetMillions,
                               colour = Genre) )

# To view a particular quadrant :
m + geom_point() +
  xlim(50,100)+
  ylim(50,100)
# It cuts data to display within specified limits. That's not what we want


#instead - ZOOM

n = ggplot(data = movies, aes(x = BudgetMillions))

n+ geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black")+
  coord_cartesian(ylim = c(0,50))


u + geom_point(aes(size = BudgetMillions)) +
  geom_smooth()+
  facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0,100))


#-------------------------------------------------------------------------------
# THEME

n = ggplot(data = movies, aes(x = BudgetMillions))
h = n + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black")


# add axis label

h + xlab("Money axis") +
  ylab("No. of Movies") +
  theme(axis.title.x = element_text(colour = "DarkGreen" , size= 20),
        axis.title.y = element_text(colour = "Red" , size= 20) ,
        axis.text.x = element_text(size = 10) , 
        axis.text.y = element_text(size = 10))

?theme

#legend formatting

h + xlab("Money axis") +
  ylab("No. of Movies") +
  theme(axis.title.x = element_text(colour = "DarkGreen" , size= 20),
        axis.title.y = element_text(colour = "Red" , size= 20) ,
        axis.text.x = element_text(size = 10) , 
        axis.text.y = element_text(size = 10) ,
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = c(1,1),
        legend.justification = c(1,1)) 

#title of the plot

h + xlab("Money axis") +
  ylab("No. of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(colour = "DarkGreen" , size= 20),
        axis.title.y = element_text(colour = "Red" , size= 20) ,
        axis.text.x = element_text(size = 10) , 
        axis.text.y = element_text(size = 10) ,
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = c(1,1),
        legend.justification = c(1,1), 
        plot.title = element_text(colour = "DarkBlue", 
                                  size = 40,
                                  family = "Courier")) 























