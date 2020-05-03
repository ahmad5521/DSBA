#=======================================================================
#
# Project 2 - Cold Storage
#
#=======================================================================
#calling all libraries that we are going to use
library(readr)
library(dplyr)
library(ggpubr)



#=======================================================================
# problem 1
#=======================================================================

#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/DSBA/Data/Project 2 - Cold Storage")


#reading data from csv file to Cold_Storage_Temp_Data variable and view it
Cold_Storage_Temp_Data <- read.csv("Cold_Storage_Temp_Data.csv")
View(Cold_Storage_Temp_Data)

#check if ther is any NA value in dataset
anyNA(Cold_Storage_Temp_Data)

#Return a summary of the dataset variables.
summary(Cold_Storage_Temp_Data)

#Retrieve the dimension of an object.
dim(Cold_Storage_Temp_Data)

#Get the names of an object.
names(Cold_Storage_Temp_Data)

#Display the internal structure of an dataset.
str(Cold_Storage_Temp_Data)

#Returns the first 10 rows of the dataset.
head(Cold_Storage_Temp_Data, 10)

#Returns the last 10 rows of the dataset.
tail(Cold_Storage_Temp_Data, 10)

#objects in the dataset can be accessed by simply giving their names
attach(Cold_Storage_Temp_Data)

#findoing mean cold storage temperature for summer,winter and Rainy Season
aggregate(x = Temperature,                # Specify data column
          by = list(Season),              # Specify group indicator
          FUN = mean)                     # Specify function (i.e. mean)

#Finding overall mean for the full year
theMean <- mean(Temperature)

#Finding Standard Deviation for the full year
theSD <- sd(Temperature)

#the probability of temperature having fallen below 2 C
pnorm(2, mean=theMean, sd=theSD, lower.tail = T)
#calculate percentage
pnorm(2, mean=theMean, sd=theSD, lower.tail = T) * 100

#the probability of temperature having gone above 4 C
pnorm(4, mean=theMean, sd=theSD, lower.tail = F)
#calculate percentage
pnorm(4, mean=theMean, sd=theSD, lower.tail = F) * 100





# one-way ANOVA
#ompute summary statistics by groups - count, mean, sd:
group_by(Cold_Storage_Temp_Data, Season) %>%
  summarise(
    count = n(),
    mean = mean(Temperature, na.rm = TRUE),
    sd = sd(Temperature, na.rm = TRUE)
  )

# Compute the analysis of variance
one.way <- aov(Temperature ~ Season, data = Cold_Storage_Temp_Data)
# Summary of the analysis
summary(one.way)
#find how the treatment levels differ from one another
TukeyHSD(one.way)
#ploting TukeHSD
plot(TukeyHSD(one.way))




#=======================================================================
# problem 2
#=======================================================================

#reading data from csv file to Cold_Storage_Mar2018 variable and view it
Cold_Storage_Mar2018 <- read_csv("Cold_Storage_Mar2018.csv")
View(Cold_Storage_Mar2018)

#check if ther is any NA value in dataset
anyNA(Cold_Storage_Mar2018)

#take look to data throgh boxplot
boxplot(Cold_Storage_Mar2018$Temperature)

#performing t-test
t.test(x=Cold_Storage_Mar2018$Temperature, mu = 3.9, alt = 'greater')

#=======================================================================
#
# T H E - E N D
#
#=======================================================================

