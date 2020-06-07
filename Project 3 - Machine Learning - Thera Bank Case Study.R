#=======================================================================
#
# Project 3 
#
#=======================================================================
#calling all libraries that we are going to use
library(readxl) # read xlsx
library(DataExplorer) # visual exploration of data
library(ggplot2)# data visualisation
library(gridExtra) # arrange multiple grid-based plots on a page
library(factoextra) # extract and visualize the results of multivariate data analysis
library(NbClust) # to find optimal number of clusters

#setting up working directory
setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Project 3 - Thera Bank Case Study")


#reading data from csv file to Thera_Bank_Personal_Loan_Modelling_Dataset variable and view it
Thera_Bank_Personal_Loan_Modelling_Dataset <- read_excel("Thera Bank_Personal_Loan_Modelling-dataset.xlsx", sheet = 2)

# fix spaces in column names
spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}
Thera_Bank_Personal_Loan_Modelling_Dataset <- spaceless(Thera_Bank_Personal_Loan_Modelling_Dataset)

# renaming some columen
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[2] <- "Age"
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[3] <- "Experiance"
names(Thera_Bank_Personal_Loan_Modelling_Dataset)[4] <- "Income"

attach(Thera_Bank_Personal_Loan_Modelling_Dataset)

View(Thera_Bank_Personal_Loan_Modelling_Dataset)

#check if ther is any NA value in dataset
anyNA(Thera_Bank_Personal_Loan_Modelling_Dataset)
anyNA(ID)               
anyNA(Age)              
anyNA(Experiance)
anyNA(Income)           
anyNA(ZIPCode)          
anyNA(Familymembers)
anyNA(CCAvg)            
anyNA(Education)        
anyNA(Mortgage)
anyNA(PersonalLoan)     
anyNA(SecuritiesAccount)
anyNA(CDAccount)
anyNA(Online)           
anyNA(CreditCard)

#dealing with NA values
Thera_Bank_Personal_Loan_Modelling_Dataset[is.na(Thera_Bank_Personal_Loan_Modelling_Dataset)] <- 0

#Converting variable tipes from int to logic and factor
Thera_Bank_Personal_Loan_Modelling_Dataset$ZIPCode = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$ZIPCode)
Thera_Bank_Personal_Loan_Modelling_Dataset$Education = as.factor(Thera_Bank_Personal_Loan_Modelling_Dataset$Education)
Thera_Bank_Personal_Loan_Modelling_Dataset$PersonalLoan = as.logical(Thera_Bank_Personal_Loan_Modelling_Dataset$PersonalLoan)

Thera_Bank_Personal_Loan_Modelling_Dataset$SecuritiesAccount = as.logical(Thera_Bank_Personal_Loan_Modelling_Dataset$SecuritiesAccount)
Thera_Bank_Personal_Loan_Modelling_Dataset$CDAccount = as.logical(Thera_Bank_Personal_Loan_Modelling_Dataset$CDAccount)
Thera_Bank_Personal_Loan_Modelling_Dataset$Online = as.logical(Thera_Bank_Personal_Loan_Modelling_Dataset$Online)
Thera_Bank_Personal_Loan_Modelling_Dataset$CreditCard = as.logical(Thera_Bank_Personal_Loan_Modelling_Dataset$CreditCard)

#attaching varioable names
attach(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Return a summary of the dataset variables.
summary(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Retrieve the dimension of an object.
dim(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Get the names of an object.
names(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Display the internal structure of an dataset.
str(Thera_Bank_Personal_Loan_Modelling_Dataset)

#Returns the first 10 rows of the dataset.
head(Thera_Bank_Personal_Loan_Modelling_Dataset, 10)

#Returns the last 10 rows of the dataset.
tail(Thera_Bank_Personal_Loan_Modelling_Dataset, 10)


# univariate Analysis

#graph for all variables
# Quantitative
par(mfrow=c(2,3))
boxplot(Age, main = "Age")
boxplot(Experiance, main = "Experiance")
boxplot(Income, main = "Income")
hist(Age, main = "Age")
hist(Experiance, main = "Experiance")
hist(Income, main = "Income")

par(mfrow=c(2,3))
boxplot(Familymembers,main = "Familymembers")
boxplot(CCAvg, main = "CCAvg")
boxplot(Mortgage,main = "Mortgage")
hist(Familymembers, main = "Familymembers")
hist(CCAvg, main = "CCAvg")
hist(Mortgage, main = "Mortgage")

# catagorical
par(mfrow=c(3,2))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = ZIPCode))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = Education))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = PersonalLoan))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = SecuritiesAccount))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = CDAccount))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = Online))
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset) + geom_bar(aes(x = CreditCard))

#Bi-Variate Analysis
#jitter and box plots
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = factor(Education, #defining x axis a categorical
                      labels = c("1", "2", "3")),
           y = Income,
           color = Education)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Costomer Income by Education",
       x = "",
       y = "Income (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


#jitter and box plots
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = factor(Education, #defining x axis a categorical
                      labels = c("1", "2", "3")),
           y = Income,
           color = Education)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Costomer Income by Education",
       x = "",
       y = "Income (k)") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Age)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Age",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Age") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Experiance)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Experiance",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Experiance") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = CCAvg)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "CCAvg",
       title = "Income(K) vs CCAvg",
       subtitle = "Relation between Customer Income and CCAvg") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

#scatter plot
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,aes(x = Income, y = Mortgage)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Mortgage",
       title = "Income(K) vs Mortgage",
       subtitle = "Relation between Customer Income and Mortgage") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables

# stacked bar chart
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = CreditCard,
           fill = factor(PersonalLoan,
                         levels = c("TRUE", "FALSE"),
                         labels = c("TRUE", "FALSE")))) +
  labs(fill = "PersonalLoan", # setting title of legend
       x = "CreditCard",
       title = "CreditCard by PersonalLoan") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# stacked bar chart
ggplot(Thera_Bank_Personal_Loan_Modelling_Dataset,
       aes(x = SecuritiesAccount,
           fill = factor(PersonalLoan,
                         levels = c("TRUE", "FALSE"),
                         labels = c("TRUE", "FALSE")))) +
  labs(fill = "PersonalLoan", # setting title of legend
       x = "SecuritiesAccount",
       title = "SecuritiesAccount by PersonalLoan") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



#=======================================================================
#2.1 Apply Clustering algorithm < type, rationale>

library(tidyverse)
library(cluster)
library(Nbclust)

Thera_Bank_Personal_Loan_Modelling_Dataset.scaled = scale(Thera_Bank_Personal_Loan_Modelling_Dataset %>% select(2, 3, 4, 6, 7))
seed = 12
set.seed(seed)
clus = kmeans(x=Thera_Bank_Personal_Loan_Modelling_Dataset.scaled, centers = 4, nstart = 5)
clusplot(Thera_Bank_Personal_Loan_Modelling_Dataset.scaled,clus$cluster, color = T, shode = T, label = 2, lines = 1)

totWss = rep(5)
for(k in 1:5){
  set.seed(seed)
  clus = kmeans(x=Thera_Bank_Personal_Loan_Modelling_Dataset.scaled, centers = k, nstart = 5)
  totWss[k] = clus$tot.withinss
}
print(totWss)
plot(c(1:5), totWss, type = "b")

set.seed(seed)
nc = NbClust(Thera_Bank_Personal_Loan_Modelling_Dataset %>% select(2, 3, 4, 6, 7), min.nc = 2, max.nc = 5, method = "kmeans")



clus = kmeans(x=Thera_Bank_Personal_Loan_Modelling_Dataset.scaled, centers = 3, nstart = 5)
clusplot(Thera_Bank_Personal_Loan_Modelling_Dataset.scaled,clus$cluster, color = T, shode = T, label = 1, lines = 1)


 #=======================================================================
#
# T H E - E N D
#
#=======================================================================

