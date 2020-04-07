

CardioGoodFitness <- read.csv("CardioGoodFitness.csv")


attach(CardioGoodFitness)
dim(CardioGoodFitness)
names(CardioGoodFitness)
str(CardioGoodFitness)
head(CardioGoodFitness)
tail(CardioGoodFitness)
summary(CardioGoodFitness)

#convert from quantitative to qualitative 
CardioGoodFitness$Fitness <- factor(CardioGoodFitness$Fitness, order = F, levels =c("1", "2", "3", "4", "5"))


#get Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#product
getmode(Product)

#Age
mean(Age)
median(Age)
getmode(Age)
#get Q1 Q3 IQR
quantile(Age, c(0.25, 0.75), type = 1)
min(Age)
max(Age)


#Gender
getmode(Gender)

#Education
mean(Education)
median(Education)
getmode(Education)
#get Q1 Q3 IQR
quantile(Education, c(0.25, 0.75), type = 1)
min(Education)
max(Education)

#MaritalStatus
getmode(MaritalStatus)

#Usage
mean(Usage)
median(Usage)
getmode(Usage)
#get Q1 Q3 IQR
quantile(Usage, c(0.25, 0.75), type = 1)
min(Usage)
max(Usage)


#Income
mean(Income)
median(Income)
getmode(Income)
#get Q1 Q3 IQR
quantile(Income, c(0.25, 0.75), type = 1)
min(Income)
max(Income)

#Miles
mean(Miles)
median(Miles)
getmode(Miles)
#get Q1 Q3 IQR
quantile(Miles, c(0.25, 0.75), type = 1)
min(Miles)
max(Miles)






############## make one graph for all######################
# Quantitative
par(mfrow=c(2,3))
boxplot(Age, main = "Age", pch=1)
boxplot(Education, main = "Education", pch=2)
boxplot(Usage, main = "Usage", pch=3)
hist(Age, main = "Age", pch=4)
hist(Education, main = "Education", pch=5)
hist(Usage, main = "Usage", pch=6)

par(mfrow=c(2,2))
boxplot(Income/1000,main = "Income(K)", pch=4)
hist(Income/1000, main = "Income(K)", pch=1)
boxplot(Miles, main = "Miles", pch=5)
hist(Miles, main = "Miles", pch=1)



# catagorical
ggplot(CardioGoodFitness) + geom_bar(aes(x = Product))
ggplot(CardioGoodFitness) + geom_bar(aes(x = Gender))
ggplot(CardioGoodFitness) + geom_bar(aes(x = MaritalStatus))
ggplot(CardioGoodFitness) + geom_bar(aes(x = Fitness))

#######################################################

# Quantitative
############# Customer Incoms by Product type################
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
library(scales)
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

############# Customer Education by Product type################
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
library(scales)
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


############# Customer Miles by Product type################
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
library(scales)
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

############# Customer Age by Product type################
#kernel density plots Customer Age by Product type
ggplot(CardioGoodFitness,
       aes(x = Age, #quantitative variable
           fill = factor(Product, #defining x axis a categorical
                         levels = c("TM195", "TM498", "TM798"),
                         labels = c("TM195", "TM498", "TM798")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "Products", # setting title of legend
       x = "Age",
       title = "Customer Age by Product type")


library(scales)
ggplot(CardioGoodFitness,
       aes(x = factor(Product, #defining x axis a categorical
                      labels = c("TM195", "TM498", "TM798")),
           y = Age,
           color = Product)) + #specifying that coloring is to be based on drive type
  geom_boxplot(size=1, #makes the lines thicker
               outlier.shape = 1, #specifies circles for outliers
               outlier.color = "black", #makes outliers black
               outlier.size = 3) + #increases the size of the outlier symbol
  geom_jitter(alpha = 0.5, #setting transparency of graph
              width=.2) + #decreases the amount of jitter (.4 is the default)
  labs(title = "Customer Age by Product type",
       x = "",
       y = "Age") +
  theme_minimal() + #setting minimal theme (no background color)
  theme(legend.position = "none") + #hiding legend
  coord_flip() #x and y axes are reversed


############# Customer Usage by Product type################
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

library(scales)
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



# catagorical
############ Custome Gender by Product type##################
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

############ Custome Fitness by Product type##################
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

############ Custome MaritalStatus by Product type##################
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
#############################################################










########Relation between Customer Income and Age########
#scatter plot
library(ggplot2) #loading the library required for this plot
ggplot(CardioGoodFitness,aes(x = Income/1000,y = Age)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
size = 2,
alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Age",
       title = "Income(K) vs Age",
       subtitle = "Relation between Customer Income and Age") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


########Relation between Customer Income and Education########
#scatter plot
library(ggplot2) #loading the library required for this plot
ggplot(CardioGoodFitness,aes(x = Income/1000,y = Miles)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income(K)", #specifying the labels of axes and title of plot
       y = "Education",
       title = "Income(K) vs Education",
       subtitle = "Relation between Customer Income and Education") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables
