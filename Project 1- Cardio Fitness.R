#=======================================================================
#
# Exploratory Data Analysis - CardioFitness
#
#=======================================================================
#calling all libraries that we are going to use
library(readr)
library(ggplot2) 
library(plyr)
library(scales)

#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Project 1-Cardio Fitness")

#reading data from csv file to CardioGoodFitness variable
CardioGoodFitness <- read.csv("CardioGoodFitness.csv")

#Retrieve the dimension of an object.
dim(CardioGoodFitness)

#Get the names of an object.
names(CardioGoodFitness)

#Display the internal structure of an dataset.
str(CardioGoodFitness)

#Returns the first 10 rows of the dataset.
head(CardioGoodFitness, 10)

#Returns the last 10 rows of the dataset.
tail(CardioGoodFitness, 10)

#Return a summary of the dataset variables.
summary(CardioGoodFitness)

#check if ther is any NA value in dataset
anyNA(CardioGoodFitness)

#convert from quantitative to qualitative 
CardioGoodFitness$Fitness <- factor(CardioGoodFitness$Fitness, order = F, levels =c("1", "2", "3", "4", "5"))
CardioGoodFitness$Fitness <- factor(mapvalues(Fitness, from = c("1", "2", "3", "4", "5"), to = c("very unfit", "unfit", "normal", "fit", "very fit")))
summary(CardioGoodFitness)

#objects in the dataset can be accessed by simply giving their names
attach(CardioGoodFitness)

#get Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# univariate Analysis
#product
#Mode
getmode(Product)

#Age
#mean
mean(Age)
#median
median(Age)
#mode
getmode(Age)
#get Q1 Q3
quantile(Age, c(0.25, 0.75), type = 1)
#IQR
unname(quantile(Age, c(0.25, 0.75), type = 1)[2] - quantile(Age, c(0.25, 0.75), type = 1)[1])
#min value
min(Age)
#max value
max(Age)

#Gender
#mode
getmode(Gender)

#Education
#mean
mean(Education)
#median
median(Education)
#mode
getmode(Education)
#get Q1 Q3
quantile(Education, c(0.25, 0.75), Education = 1)
#IQR
unname(quantile(Education, c(0.25, 0.75), type = 1)[2] - quantile(Education, c(0.25, 0.75), type = 1)[1])
#min value
min(Education)
#max value
max(Education)

#MaritalStatus
#mode
getmode(MaritalStatus)

#Usage
#mean
mean(Usage)
#median
median(Usage)
#mode
getmode(Usage)
#get Q1 Q3
quantile(Usage, c(0.25, 0.75), Usage = 1)
#IQR
unname(quantile(Usage, c(0.25, 0.75), type = 1)[2] - quantile(Usage, c(0.25, 0.75), type = 1)[1])
#min value
min(Usage)
#max value
max(Usage)

#Fitness
#mode
getmode(Fitness)

#Income
#mean
mean(Income)
#median
median(Income)
#mode
getmode(Income)
#get Q1 Q3
quantile(Income, c(0.25, 0.75), Income = 1)
#IQR
unname(quantile(Income, c(0.25, 0.75), type = 1)[2] - quantile(Income, c(0.25, 0.75), type = 1)[1])
#min value
min(Income)
#max value
max(Income)

#Miles
#mean
mean(Miles)
#median
median(Miles)
#mode
getmode(Miles)
#get Q1 Q3
quantile(Miles, c(0.25, 0.75), type = 1)
#IQR
unname(quantile(Miles, c(0.25, 0.75), type = 1)[2] - quantile(Miles, c(0.25, 0.75), type = 1)[1])
#min value
min(Miles)
#max value
max(Miles)

#graph for all variable variables
# Quantitative
par(mfrow=c(2,3))
boxplot(Age, main = "Age")
boxplot(Education, main = "Education")
boxplot(Usage, main = "Usage")
hist(Age, main = "Age")
hist(Education, main = "Education")
hist(Usage, main = "Usage")

par(mfrow=c(2,2))
boxplot(Income/1000,main = "Income(K)")
boxplot(Miles, main = "Miles")
hist(Income/1000, main = "Income(K)")
hist(Miles, main = "Miles")

# catagorical
par(mfrow=c(2,2))
ggplot(CardioGoodFitness) + geom_bar(aes(x = Product))
ggplot(CardioGoodFitness) + geom_bar(aes(x = Gender))
ggplot(CardioGoodFitness) + geom_bar(aes(x = MaritalStatus))
ggplot(CardioGoodFitness) + geom_bar(aes(x = Fitness))


#Bi-Variate Analysis
# Customer Age by Fitness
#kernel density plots
ggplot(CardioGoodFitness,
       aes(x = Age, #quantitative variable
           fill = factor(Fitness, #defining x axis a categorical
                         levels = c("very unfit", "unfit", "normal", "fit", "very fit"),
                         labels = c("very unfit", "unfit", "normal", "fit", "very fit")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Fitness", # setting title of legend
       x = "Age)",
       title = "Customer Age by Fitness")

# Customer Incoms by Product type
#kernel density plots
ggplot(CardioGoodFitness,
       aes(x = Income/1000, #quantitative variable
           fill = factor(Product, #defining x axis a categorical
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Products", # setting title of legend
       x = "Income (K)",
       title = "Customer Incoms by Product type")

#jitter and box plots
ggplot(CardioGoodFitness,
       aes(x = factor(Product, #defining x axis a categorical
                      labels = c("TM195", "TM498", "TM798")),
           y = Income/1000,
           color = Product)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Costomer Income by Product Type",
       x = "",
       y = "Income (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed

#Customer Education by Product type
#kernel density plots 
ggplot(CardioGoodFitness,
       aes(x = Education, #quantitative variable
           fill = factor(Product, #defining x axis a categorical
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Products", # setting title of legend
       x = "Education",
       title = "Customer Education by Product type")

#jitter and box plots
ggplot(CardioGoodFitness,
       aes(x = factor(Product, #defining x axis a categorical
                      labels = c("TM195", "TM498", "TM798")),
           y = Education,
           color = Product)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Customer Education by Product type",
       x = "",
       y = "Education") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#Customer Miles by Product type
#kernel density plots Customer Miles by Product type
ggplot(CardioGoodFitness,
       aes(x = Miles, #quantitative variable
           fill = factor(Product, #defining x axis a categorical
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Products", # setting title of legend
       x = "Miles",
       title = "Customer Miles by Product type")

#jitter and box plots
ggplot(CardioGoodFitness,
       aes(x = factor(Product, #defining x axis a categorical
                      labels = c("TM195", "TM498", "TM798")),
           y = Miles,
           color = Product)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Customer Miles by Product type",
       x = "",
       y = "Miles") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#Customer Usage by Product type
#kernel density plots Customer Usage by Product type
ggplot(CardioGoodFitness,
       aes(x = Usage, #quantitative variable
           fill = factor(Product, #defining x axis a categorical
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Products", # setting title of legend
       x = "Usage",
       title = "Customer avrage Usage by Product type")


ggplot(CardioGoodFitness,
       aes(x = factor(Product, #defining x axis a categorical
                      labels = c("TM195", "TM498", "TM798")),
           y = Usage,
           color = Product)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Customer Avrage Usage by Product type",
       x = "",
       y = "Usage") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed



#Custome Gender by Product type
# stacked bar chart
ggplot(CardioGoodFitness,
       aes(x = Gender,
           fill = factor(Product,
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  labs(fill = "Products", # setting title of legend
       x = "Gender",
       title = "Custome Gender by Product type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

#Custome Fitness by Product type
# stacked bar chart
ggplot(CardioGoodFitness,
       aes(x = Fitness,
           fill = factor(Product,
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  labs(fill = "Products", # setting title of legend
       x = "Fitness",
       title = "Custome Fitness by Product type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

#Custome MaritalStatus by Product type
# stacked bar chart
ggplot(CardioGoodFitness,
       aes(x = MaritalStatus,
           fill = factor(Product,
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  labs(fill = "Products", # setting title of legend
       x = "Marital Status",
       title = "Custome Marital Status by Product type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

#Relation between Customer Income and Age
#scatter plot
ggplot(CardioGoodFitness,aes(x = Income/1000,y = Age)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Age",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Age") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#Relation between Customer Income and Education
#scatter plot
ggplot(CardioGoodFitness,aes(x = Income/1000,y = Education)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Education",
       title = "Income(K) vs Education",
       subtitle = "Relation between Customer Income and Education") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#Relation between Customer Age and Miles
#scatter plot
ggplot(CardioGoodFitness,aes(x = Age,y = Miles)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Age", #specifying the labels of axes and title of plot
       y = "Miles",
       title = "Age vs Miles",
       subtitle = "Relation between Customer Income and Miles") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#Relation between Customer Age and Usage
#scatter plot
ggplot(CardioGoodFitness,aes(x = Age,y = Usage)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Age", #specifying the labels of axes and title of plot
       y = "Usage",
       title = "Age vs Usage",
       subtitle = "Relation between Customer Age and Usage") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#outlier identification
#Income
boxplot.stats(CardioGoodFitness$Income)$out 

#Age
boxplot.stats(CardioGoodFitness$Age)$out 

#Usage
boxplot.stats(CardioGoodFitness$Usage)$out 

#Education
boxplot.stats(CardioGoodFitness$Education)$out 

#Miles
boxplot.stats(CardioGoodFitness$Miles)$out 

#=======================================================================
#
# T H E - E N D
#
#=======================================================================

