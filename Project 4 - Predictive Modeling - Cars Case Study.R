#=======================================================================
#
# Project 4 
#
#=======================================================================
#calling all libraries that we are going to use
library(readr)
library(ggplot2) 


#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Project 4 - Cars Case Study")
#reading data from csv file to Cars variable and view it
Cars <- read.csv("Cars-dataset.csv")
attach(Cars)



#Prepare the data for analysis
Cars$Engineer = as.factor(Engineer)
Cars$MBA = as.factor(MBA)
Cars$license = as.factor(license)

#dealing with NA values in MBA variable
Cars[is.na(Cars)] <- 1


#check if ther is any NA value in dataset
anyNA(Cars)


#outlier identification
#Salary
boxplot.stats(Cars$Salary)$out 
#Age
boxplot.stats(Cars$Age)$out 
#Work.Exp
boxplot.stats(Cars$Work.Exp)$out 
#Distance
boxplot.stats(Cars$Distance)$out

# EDA 

#Retrieve the dimension of an object.
dim(Cars)

#Get the names of an object.
names(Cars)

#Display the internal structure of an dataset.
str(Cars)

#Returns the first 10 rows of the dataset.
head(Cars, 10)

#Returns the last 10 rows of the dataset.
tail(Cars, 10)

#Return a summary of the dataset variables.
summary(Cars)





#graph for all variable variables
# Quantitative
par(mfrow=c(2,2))
boxplot(Age, main = "Age")
boxplot(Work.Exp, main = "Work.Exp")
hist(Age, main = "Age")
hist(Work.Exp, main = "Work.Exp")

par(mfrow=c(2,2))
boxplot(Salary,main = "Salary(K)")
boxplot(Distance, main = "Distance")
hist(Salary, main = "Salary(K)")
hist(Distance, main = "Distance")

# catagorical
par(mfrow=c(2,2))
ggplot(Cars) + geom_bar(aes(x = Gender))
ggplot(Cars) + geom_bar(aes(x = Engineer))
ggplot(Cars) + geom_bar(aes(x = MBA))
ggplot(Cars) + geom_bar(aes(x = license))
ggplot(Cars) + geom_bar(aes(x = Transport))


#Bi-Variate Analysis
#kernel density plots
ggplot(Cars,
       aes(x = Age, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = 0.8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Age",
       title = "Employee Age by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Age,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Age by Transport Type",
       x = "",
       y = "Age") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Salary, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Salary",
       title = "Employee Salary by Transport")

#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Salary,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Salary by Transport Type",
       x = "",
       y = "Salary (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Work.Exp, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Work.Exp",
       title = "Employee Work.Exp by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Work.Exp,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Work.Exp by Transport Type",
       x = "",
       y = "Work.Exp") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#kernel density plots
ggplot(Cars,
       aes(x = Distance, #quantitative variable
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  geom_density(alpha = .8) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Transport", # setting title of legend
       x = "Distance",
       title = "Employee Distance by Transport")
#jitter and box plots
ggplot(Cars,
       aes(x = factor(Transport, #defining x axis a categorical
                      labels = c("2Wheeler", "Car", "Public Transport")),
           y = Distance,
           color = Transport)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Employee Distance by Transport Type",
       x = "",
       y = "Distance") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed




# stacked bar chart
ggplot(Cars,
       aes(x = Engineer,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "Engineer",
       title = "Employee Engineer by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# stacked bar chart
ggplot(Cars,
       aes(x = MBA,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "MBA",
       title = "Employee MBA by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


# stacked bar chart
ggplot(Cars,
       aes(x = license,
           fill = factor(Transport, #defining x axis a categorical
                         levels = c("2Wheeler", "Car", "Public Transport"),
                         labels = c("2Wheeler", "Car", "Public Transport")))) +
  labs(fill = "Transport", # setting title of legend
       x = "license",
       title = "Employee license by Transport") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



