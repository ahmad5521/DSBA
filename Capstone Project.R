library(readxl)
library(plyr)
library(ggplot2)

setwd("C:/Users/ahmasiri/Desktop/PGP DSBA/Data/Capstone Project")


Insurance_Premium_Default_Dataset <- read_excel("Insurance Premium Default-Dataset.xlsx")




#Retrieve the dimension of an object.
dim(Insurance_Premium_Default_Dataset)

#Get the names of an object.
names(Insurance_Premium_Default_Dataset)

#Display the internal structure of an dataset.
str(Insurance_Premium_Default_Dataset)

#Returns the first 10 rows of the dataset.
head(Insurance_Premium_Default_Dataset, 10)

#Returns the last 10 rows of the dataset.
tail(Insurance_Premium_Default_Dataset, 10)

#Return a summary of the dataset variables.
summary(Insurance_Premium_Default_Dataset)

#check if ther is any NA value in dataset
anyNA(Insurance_Premium_Default_Dataset)


#preparing variables

#generate age_in_years
Insurance_Premium_Default_Dataset$age_in_years = as.integer(format(round(Insurance_Premium_Default_Dataset$age_in_days/360, 0), nsmall = 0))



#convert from quantitative to qualitative 

#Marital_Status
Insurance_Premium_Default_Dataset$`Marital Status` <- factor(Insurance_Premium_Default_Dataset$`Marital Status`, order = F, levels =c("1","0"))
Insurance_Premium_Default_Dataset$Marital_Status <- factor(mapvalues(Insurance_Premium_Default_Dataset$`Marital Status`, from = c("1", "0"), to = c("Married","Unmarried")))


#Accomodation
Insurance_Premium_Default_Dataset$Accomodation <- factor(Insurance_Premium_Default_Dataset$Accomodation, order = F, levels =c("1","0"))
Insurance_Premium_Default_Dataset$Accomodation <- factor(mapvalues(Insurance_Premium_Default_Dataset$Accomodation, from = c("1", "0"), to = c("Owned","Rented")))

#sourcing_channel
Insurance_Premium_Default_Dataset$sourcing_channel = as.factor(Insurance_Premium_Default_Dataset$sourcing_channel)

#residence_area_type
Insurance_Premium_Default_Dataset$residence_area_type = as.factor(Insurance_Premium_Default_Dataset$residence_area_type)


#`Count_3-6_months_late`
Insurance_Premium_Default_Dataset$`Count_3-6_months_late` = as.factor(Insurance_Premium_Default_Dataset$`Count_3-6_months_late`)

#`Count_6-12_months_late`
Insurance_Premium_Default_Dataset$`Count_6-12_months_late` = as.factor(Insurance_Premium_Default_Dataset$`Count_6-12_months_late`)

#Count_more_than_12_months_late
Insurance_Premium_Default_Dataset$Count_more_than_12_months_late = as.factor(Insurance_Premium_Default_Dataset$Count_more_than_12_months_late)

#default
Insurance_Premium_Default_Dataset$default = as.factor(Insurance_Premium_Default_Dataset$default)
Insurance_Premium_Default_Dataset$default <- factor(mapvalues(Insurance_Premium_Default_Dataset$default, from = c("1", "0"), to = c("1", "0")))


#objects in the dataset can be accessed by simply giving their names
attach(Insurance_Premium_Default_Dataset)


summary(Insurance_Premium_Default_Dataset)



# Load DataExplorer for exploratory data analysis. 
library(DataExplorer)

# This function helps to visualize data structure in network graph format.
plot_str(Insurance_Premium_Default_Dataset, type="d", fontSize = 25)
# plot missing data
plot_missing(Insurance_Premium_Default_Dataset)


# Check the fivenumber summary of variables
summary(fivenum(Insurance_Premium_Default_Dataset$age_in_years))
summary(fivenum(Insurance_Premium_Default_Dataset$premium))
summary(fivenum(Insurance_Premium_Default_Dataset$no_of_premiums_paid))
summary(fivenum(Insurance_Premium_Default_Dataset$risk_score))
summary(fivenum(Insurance_Premium_Default_Dataset$No_of_dep))
summary(fivenum(Insurance_Premium_Default_Dataset$Veh_Owned))
summary(fivenum(Insurance_Premium_Default_Dataset$Income))





### UNIVARIATE ANALYSIS

library(ggplot2)
library(grid)
library(gridExtra)
## visualize properties of all categorical variables


# Setting up the aesthetics
unipar = theme(legend.position = "none") + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col1 = "Set2"

# Plotting the bar charts
g1=ggplot(Insurance_Premium_Default_Dataset, aes(x=default, fill=default)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g2=ggplot(Insurance_Premium_Default_Dataset, aes(x=residence_area_type, fill=residence_area_type)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) 

# Plotting the bar charts
g3=ggplot(Insurance_Premium_Default_Dataset, aes(x=sourcing_channel, fill=sourcing_channel)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g4=ggplot(Insurance_Premium_Default_Dataset, aes(x=Accomodation, fill=Accomodation)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) 

# Plotting the bar charts
g5=ggplot(Insurance_Premium_Default_Dataset, aes(x=Marital_Status, fill=`Marital_Status`)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g6=ggplot(Insurance_Premium_Default_Dataset, aes(x=`Count_3-6_months_late`, fill=`Count_3-6_months_late`)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g7=ggplot(Insurance_Premium_Default_Dataset, aes(x=`Count_6-12_months_late`, fill=`Count_6-12_months_late`)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)




# Plotting the bar charts
g8=ggplot(Insurance_Premium_Default_Dataset) + geom_bar(aes(x = Count_more_than_12_months_late)) + scale_fill_brewer(palette=col1)


# Partitioning the barcharts
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol=2)



### BIVARIATE ANALYSIS


# Setting up the aesthetics
bipar1 = theme(legend.position = "none") + theme_light() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col2 = "Set2"

# default vs numerical variables
p1=ggplot(Insurance_Premium_Default_Dataset,
          aes(x = age_in_years, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "age_in_years")


p2=ggplot(Insurance_Premium_Default_Dataset,
          aes(x = premium, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "premium")


p3=ggplot(Insurance_Premium_Default_Dataset,
          aes(x = no_of_premiums_paid, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "no_of_premiums_paid")


p4=ggplot(Insurance_Premium_Default_Dataset,
          aes(x = risk_score, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "risk_score")

# Partitioning the boxplots
grid.arrange(p1,p2,p3,p4,ncol=2)




# Setting up the aesthetics
bipar2 = theme(legend.position = "top", 
               legend.direction = "horizontal", 
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))


library(dplyr)

# default vs categorical variables
# stacked bar chart
p8 = ggplot(Insurance_Premium_Default_Dataset,
            aes(x = residence_area_type,
                fill = factor(default,
                              levels = c("0", "Default"),
                              labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


p9 = ggplot(Insurance_Premium_Default_Dataset,
            aes(x = sourcing_channel,
                fill = factor(default,
                              levels = c("0", "Default"),
                              labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "sourcing_channel") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


p10 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = Accomodation,
                 fill = factor(default,
                               levels = c("0", "Default"),
                               labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

p11 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = Marital_Status,
                 fill = factor(default,
                               levels = c("0", "Default"),
                               labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "Marital_Status") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# Partitioning the boxplots
grid.arrange(p8,p9,p10,p11,ncol=2)



# correlation analysis
#scatter plot
c1 = ggplot(Insurance_Premium_Default_Dataset,aes(x = risk_score,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c2 = ggplot(Insurance_Premium_Default_Dataset,aes(x = risk_score,y = premium)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
c3 = ggplot(Insurance_Premium_Default_Dataset,aes(x = risk_score,y = Income)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "Income") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c4 = ggplot(Insurance_Premium_Default_Dataset,aes(x = risk_score,y = age_in_years)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "age_in_years") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c5 = ggplot(Insurance_Premium_Default_Dataset,aes(x = age_in_years,y = premium)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "age_in_years", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
c6 = ggplot(Insurance_Premium_Default_Dataset,aes(x = age_in_years,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "age_in_years", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables




#scatter plot
c7 = ggplot(Insurance_Premium_Default_Dataset,aes(x = Income/1000,y = premium/1000)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables





#scatter plot
c8 = ggplot(Insurance_Premium_Default_Dataset,aes(x = Income/1000,y = age_in_years)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "age_in_years") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c9 = ggplot(Insurance_Premium_Default_Dataset,aes(x = Income/1000,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



grid.arrange(c1,c2,c3,c4,c5,c6,c7,c8,c9,ncol=3)



# stacked bar chart
cc1 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = Accomodation,
                 fill = factor(sourcing_channel))) +
  labs(fill = "sourcing_channel", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc2 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = Accomodation,
                 fill = factor(residence_area_type))) +
  labs(fill = "residence_area_type", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc3 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = Accomodation,
                 fill = factor(Marital_Status))) +
  labs(fill = "Marital_Status", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc4 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = residence_area_type,
                 fill = factor(Marital_Status))) +
  labs(fill = "Marital_Status", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc5 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = residence_area_type,
                 fill = factor(sourcing_channel))) +
  labs(fill = "sourcing_channel", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc6 = ggplot(Insurance_Premium_Default_Dataset,
             aes(x = sourcing_channel,
                 fill = factor(residence_area_type))) +
  labs(fill = "residence_area_type", # setting title of legend
       x = "sourcing_channel") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

grid.arrange(cc1,cc2,cc3,cc4,cc5,cc6,ncol=2)



# removing unwante
IDataset = Insurance_Premium_Default_Dataset[,c(2, 4 ,5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 17 ) ]


#renaming
IDataset = IDataset %>% 
  rename(
    Count_3_6_months_late = `Count_3-6_months_late`,
    Count_6_12_months_late = `Count_6-12_months_late`
  )


attach(IDataset)

#outlier treatment


#income
lower_bound <- quantile(IDataset$Income, 0.01)
upper_bound <- quantile(IDataset$Income, 0.99)

outlier_ind <- which(IDataset$Income < lower_bound | IDataset$Income > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]




#perc_premium_paid_by_cash_credit
lower_bound <- quantile(IDataset$perc_premium_paid_by_cash_credit, 0.01)
upper_bound <- quantile(IDataset$perc_premium_paid_by_cash_credit, 0.99)

outlier_ind <- which(IDataset$perc_premium_paid_by_cash_credit < lower_bound | IDataset$perc_premium_paid_by_cash_credit > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]




# #Count_3_6_months_late
# lower_bound <- quantile(IDataset$Count_3_6_months_late, 0.01)
# upper_bound <- quantile(IDataset$Count_3_6_months_late, 0.99)

# outlier_ind <- which(IDataset$Count_3_6_months_late < lower_bound | IDataset$Count_3_6_months_late > upper_bound)

# if( length(outlier_ind) > 0)
#   IDataset = IDataset[-outlier_ind, ]




# #Count_6_12_months_late
# lower_bound <- quantile(IDataset$Count_6_12_months_late, 0.01)
# upper_bound <- quantile(IDataset$Count_6_12_months_late, 0.99)

# outlier_ind <- which(IDataset$Count_6_12_months_late < lower_bound | IDataset$Count_6_12_months_late > upper_bound)

# if( length(outlier_ind) > 0)
#   IDataset = IDataset[-outlier_ind, ]





# #Count_more_than_12_months_late
# lower_bound <- quantile(IDataset$Count_more_than_12_months_late, 0.01)
# upper_bound <- quantile(IDataset$Count_more_than_12_months_late, 0.99)

# outlier_ind <- which(IDataset$Count_more_than_12_months_late < lower_bound | IDataset$Count_more_than_12_months_late > upper_bound)

# if( length(outlier_ind) > 0)
#   IDataset = IDataset[-outlier_ind, ]




#Veh_Owned
lower_bound <- quantile(IDataset$Veh_Owned, 0.01)
upper_bound <- quantile(IDataset$Veh_Owned, 0.99)

outlier_ind <- which(IDataset$Veh_Owned < lower_bound | IDataset$Veh_Owned > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]



#No_of_dep
lower_bound <- quantile(IDataset$No_of_dep, 0.01)
upper_bound <- quantile(IDataset$No_of_dep, 0.99)

outlier_ind <- which(IDataset$No_of_dep < lower_bound | IDataset$No_of_dep > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]





#risk_score
lower_bound <- quantile(IDataset$risk_score, 0.01)
upper_bound <- quantile(IDataset$risk_score, 0.99)

outlier_ind <- which(IDataset$risk_score < lower_bound | IDataset$risk_score > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]



#no_of_premiums_paid
lower_bound <- quantile(IDataset$no_of_premiums_paid, 0.01)
upper_bound <- quantile(IDataset$no_of_premiums_paid, 0.99)

outlier_ind <- which(IDataset$no_of_premiums_paid < lower_bound | IDataset$no_of_premiums_paid > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]






#premium
lower_bound <- quantile(IDataset$premium, 0.01)
upper_bound <- quantile(IDataset$premium, 0.99)

outlier_ind <- which(IDataset$premium < lower_bound | IDataset$premium > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]



#age_in_years
lower_bound <- quantile(IDataset$age_in_years, 0.01)
upper_bound <- quantile(IDataset$age_in_years, 0.99)

outlier_ind <- which(IDataset$age_in_years < lower_bound | IDataset$age_in_years > upper_bound)

if( length(outlier_ind) > 0)
  IDataset = IDataset[-outlier_ind, ]

#EDA again

# Check the fivenumber summary of variables
summary(fivenum(IDataset$age_in_years))
summary(fivenum(IDataset$premium))
summary(fivenum(IDataset$no_of_premiums_paid))
summary(fivenum(IDataset$risk_score))
summary(fivenum(IDataset$No_of_dep))
summary(fivenum(IDataset$Veh_Owned))
summary(fivenum(IDataset$Income))





### UNIVARIATE ANALYSIS

library(ggplot2)
library(grid)
library(gridExtra)
## visualize properties of all categorical variables


# Setting up the aesthetics
unipar = theme(legend.position = "none") + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col1 = "Set2"

# Plotting the bar charts
g1=ggplot(IDataset, aes(x=default, fill=default)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g2=ggplot(IDataset, aes(x=residence_area_type, fill=residence_area_type)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) 

# Plotting the bar charts
g3=ggplot(IDataset, aes(x=sourcing_channel, fill=sourcing_channel)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g4=ggplot(IDataset, aes(x=Accomodation, fill=Accomodation)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) 

# Plotting the bar charts
g5=ggplot(IDataset, aes(x=Marital_Status, fill=Marital_Status)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g6=ggplot(IDataset, aes(x=Count_3_6_months_late, fill=Count_3_6_months_late)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)

# Plotting the bar charts
g7=ggplot(IDataset, aes(x=Count_6_12_months_late, fill=Count_6_12_months_late)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1)




# Plotting the bar charts
g8=ggplot(IDataset) + geom_bar(aes(x = Count_more_than_12_months_late)) + scale_fill_brewer(palette=col2)


# Partitioning the barcharts
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol=2)



### BIVARIATE ANALYSIS


# Setting up the aesthetics
bipar1 = theme(legend.position = "none") + theme_light() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col2 = "Set2"

# default vs numerical variables
p1=ggplot(IDataset,
          aes(x = age_in_years, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "age_in_years")


p2=ggplot(IDataset,
          aes(x = premium, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "premium")


p3=ggplot(IDataset,
          aes(x = no_of_premiums_paid, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "no_of_premiums_paid")


p4=ggplot(IDataset,
          aes(x = risk_score, #quantitative variable
              fill = factor(default,
                            levels = c("0", "Default"),
                            labels = c("0", "Default")))) +
  geom_density(alpha = 0.2) + #setting transparency of graph to keep overlaps visible
  labs(fill = "default", # setting title of legend
       x = "risk_score")

# Partitioning the boxplots
grid.arrange(p1,p2,p3,p4,ncol=2)




# Setting up the aesthetics
bipar2 = theme(legend.position = "top", 
               legend.direction = "horizontal", 
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))


library(dplyr)

# default vs categorical variables
# stacked bar chart
p8 = ggplot(IDataset,
            aes(x = residence_area_type,
                fill = factor(default,
                              levels = c("0", "Default"),
                              labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


p9 = ggplot(IDataset,
            aes(x = sourcing_channel,
                fill = factor(default,
                              levels = c("0", "Default"),
                              labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "sourcing_channel") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


p10 = ggplot(IDataset,
             aes(x = Accomodation,
                 fill = factor(default,
                               levels = c("0", "Default"),
                               labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

p11 = ggplot(IDataset,
             aes(x = Marital_Status,
                 fill = factor(default,
                               levels = c("0", "Default"),
                               labels = c("0", "Default")))) +
  labs(fill = "default", # setting title of legend
       x = "Marital_Status") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

# Partitioning the boxplots
grid.arrange(p8,p9,p10,p11,ncol=2)



# correlation analysis
#scatter plot
c1 = ggplot(IDataset,aes(x = risk_score,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c2 = ggplot(IDataset,aes(x = risk_score,y = premium)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
c3 = ggplot(IDataset,aes(x = risk_score,y = Income)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "Income") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c4 = ggplot(IDataset,aes(x = risk_score,y = age_in_years)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "risk_score", #specifying the labels of axes and title of plot
       y = "age_in_years") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c5 = ggplot(IDataset,aes(x = age_in_years,y = premium)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "age_in_years", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables


#scatter plot
c6 = ggplot(IDataset,aes(x = age_in_years,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "age_in_years", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables




#scatter plot
c7 = ggplot(IDataset,aes(x = Income/1000,y = premium/1000)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "premium") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables





#scatter plot
c8 = ggplot(IDataset,aes(x = Income/1000,y = age_in_years)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "age_in_years") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



#scatter plot
c9 = ggplot(IDataset,aes(x = Income/1000,y = no_of_premiums_paid)) +
  geom_point(color="cornflowerblue", #setting the colour, size and transparency(alpha) of the points
             size = 2,
             alpha=.8) +
  labs(x = "Income", #specifying the labels of axes and title of plot
       y = "no_of_premiums_paid") +
  geom_smooth(method = "lm") # this adds a linear trend line which is useful to summarize the relationship between the two variables



grid.arrange(c1,c2,c3,c4,c5,c6,c7,c8,c9,ncol=3)



# stacked bar chart
cc1 = ggplot(IDataset,
             aes(x = Accomodation,
                 fill = factor(sourcing_channel))) +
  labs(fill = "sourcing_channel", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc2 = ggplot(IDataset,
             aes(x = Accomodation,
                 fill = factor(residence_area_type))) +
  labs(fill = "residence_area_type", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc3 = ggplot(IDataset,
             aes(x = Accomodation,
                 fill = factor(Marital_Status))) +
  labs(fill = "Marital_Status", # setting title of legend
       x = "Accomodation") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc4 = ggplot(IDataset,
             aes(x = residence_area_type,
                 fill = factor(Marital_Status))) +
  labs(fill = "Marital_Status", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc5 = ggplot(IDataset,
             aes(x = residence_area_type,
                 fill = factor(sourcing_channel))) +
  labs(fill = "sourcing_channel", # setting title of legend
       x = "residence_area_type") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked



cc6 = ggplot(IDataset,
             aes(x = sourcing_channel,
                 fill = factor(residence_area_type))) +
  labs(fill = "residence_area_type", # setting title of legend
       x = "sourcing_channel") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

grid.arrange(cc1,cc2,cc3,cc4,cc5,cc6,ncol=2)











seed = 134

set.seed(seed)
index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
IDataset.train1 = IDataset[index,]
IDataset.test1 = IDataset[-index,]


library(randomForest)

seed=1000

set.seed(seed)




#auc -> 0.1714639
#Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late    
#IDataset.train1[, c(2,3,4,5)]


#auc -> 0.0501028
#Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score    
#IDataset.train1[, c(2,3,4,5,9)]


#auc -> 0.03329917
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score   
#IDataset.train1[, c(14, 2,3,4,5,9]

#auc -> 0.05851644
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late    
#IDataset.train1[, c(14, 2,3,4,5)]


#auc -> 0.03141114
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+sourcing_channel+risk_score    
#c(14, 2,3,4,5,11,9)


#auc -> 0.0266754
#age_in_years+Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+sourcing_channel+premium+risk_score    
#c(14, 2,3,4,5,11,9,13)



#auc -> 0.01334393
#All

#build our random forest
rndFor = randomForest(default ~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score+age_in_years+perc_premium_paid_by_cash_credit, data = IDataset.train1 , 
                      ntree=501, mtry = 3, nodesize = 10,
                      importance=TRUE)

#The error rate plot w.r.t number of trees reveals that anything more than, say 51
# trees is really not that valuable.
rndFor$err.rate

plot(rndFor, main="")

legend("topright", c("OOB", "1", "0"), text.col=1:6, lty=1:3, col=1:3)

title(main="Error Rates Random Forest IDataset.train ")

set.seed(seed)


#Now we will "tune" the Random Forest by trying different m values.
#We will stick with 51 trees (odd number of trees are preferable). 
#The returned forest, "tRndFor" is the one corresponding to the best m
tRndFor = tuneRF(x = IDataset.train1[, c(1,2,3,4,5,9,14)], 
                 y= IDataset.train1 $default,
                 mtryStart = 3, 
                 ntreeTry = 251, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 10, 
                 importance=TRUE
)

#List the importance of the variables. Larger the MeanDecrease values
#the more important the variable.
importance(tRndFor)


#Lets make predictions on the training data and measure the prediction error rate.
IDataset.train1 $predict.class = predict(tRndFor, IDataset.train1 , type="class")

IDataset.train1 $prob1 = predict(tRndFor, IDataset.train1 , type="prob")[,"0"]

tb.train=table(IDataset.train1 $default, IDataset.train1 $predict.class)


confusionMatrix(IDataset.train1 $predict.class, IDataset.train1 $default, dnn = c("Prediction", "Reference"))


print('accuracy is ')
sum(diag(tb.train))/sum(tb.train)

library(ROCR)
pred_ROCR <- prediction(IDataset.train1 $prob1, IDataset.train1 $default)
roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]


print('AUC is ')
auc_ROCR




trainsensitivity2 = tb.train[2,2] / sum(tb.train[2, ])
trainsensitivity2

trainspecificity2 = tb.train[1,1] / sum(tb.train[1, ])
trainspecificity2

#Now using the tuned Random Forest from the previous step, 
#and redo our errors and top decile calculations for the previously identified threshold. 

IDataset.test1$predict.class = predict(tRndFor, IDataset.test1 , type="class")

IDataset.test1$prob1 = predict(tRndFor, IDataset.test1 , type="prob")[,"0"]

tb.test=table(IDataset.test1 $default, IDataset.test1 $predict.class)


confusionMatrix(IDataset.test1 $predict.class, IDataset.test1 $default, dnn = c("Prediction", "Reference"))


print('accuracy is ')
sum(diag(tb.test))/sum(tb.test)




testsensitivity2 = tb.test[2,2] / sum(tb.test[2, ])
testsensitivity2

testspecificity2 = tb.test[1,1] / sum(tb.test[1, ])
testspecificity2


#result
#train accuracy = 95
#train sensitivity = 99
#train specificity = 20


#test accuracy = 94
#train sensitivity = 99
#train specificity = 11




##########################################
#with smoote
##########################################

# library(DMwR)

# set.seed(seed)
# index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
# IDataset.train1 = IDataset[index,]
# IDataset.test1 = IDataset[-index,]


# ## Smote : Synthetic Minority Oversampling Technique To Handle default Imbalancy In Binary Classification
# IDataset.train.balanced.data1 <- SMOTE(default ~., as.data.frame(IDataset.train1[, c(2,3,4,5,9,16)]), perc.over = 4800, k = 5, perc.under = 1000)

# as.data.frame(table(IDataset.train.balanced.data1$default))



# ## Smote : Synthetic Minority Oversampling Technique To Handle default Imbalancy In Binary Classification
# IDataset.test.balanced.data1 <- SMOTE(default ~., as.data.frame(IDataset.test1[, c(2,3,4,5,9,16)]), perc.over = 4800, k = 5, perc.under = 1000)

# as.data.frame(table(IDataset.test.balanced.data1$default))





# library(randomForest)

# seed=1000

# set.seed(seed)



# #build our random forest
# rndFor = randomForest(default ~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score, data = IDataset.train.balanced.data1 , 
#                       ntree=501, mtry = 3, nodesize = 10,
#                       importance=TRUE)

# #The error rate plot w.r.t number of trees reveals that anything more than, say 51
# # trees is really not that valuable.
# rndFor$err.rate

# plot(rndFor, main="")

# legend("topright", c("OOB", "1", "0"), text.col=1:6, lty=1:3, col=1:3)

# title(main="Error Rates Random Forest IDataset.train ")

# set.seed(seed)

# head(IDataset.train.balanced.data1)
# names(IDataset.train.balanced.data1)
# #Now we will "tune" the Random Forest by trying different m values.
# #We will stick with 51 trees (odd number of trees are preferable). 
# #The returned forest, "tRndFor" is the one corresponding to the best m
# tRndFor = tuneRF(x = IDataset.train.balanced.data1[, -6], 
#                  y= IDataset.train.balanced.data1 $default,
#                  mtryStart = 3, 
#                  ntreeTry = 251, 
#                  stepFactor = 1.5, 
#                  improve = 0.0001, 
#                  trace=TRUE, 
#                  plot = TRUE,
#                  doBest = TRUE,
#                  nodesize = 10, 
#                  importance=TRUE
# )

# #List the importance of the variables. Larger the MeanDecrease values
# #the more important the variable.
# importance(tRndFor)


# #Lets make predictions on the training data and measure the prediction error rate.
# IDataset.train.balanced.data1 $predict.class = predict(tRndFor, IDataset.train.balanced.data1 , type="class")

# IDataset.train.balanced.data1 $prob1 = predict(tRndFor, IDataset.train.balanced.data1 , type="prob")[,"0"]

# tbl=table(IDataset.train.balanced.data1 $default, IDataset.train.balanced.data1 $predict.class)


# print('accuracy is ')
# sum(diag(tbl))/sum(tbl)

# library(ROCR)
# pred_ROCR <- prediction(IDataset.train.balanced.data1 $prob1, IDataset.train.balanced.data1 $default)
# roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
# plot(roc_ROCR, main = "ROC curve", colorize = T)
# abline(a = 0, b = 1)

# auc_ROCR <- performance(pred_ROCR, measure = "auc")
# auc_ROCR <- auc_ROCR@y.values[[1]]


# print('AUC is ')
# auc_ROCR

# #Now using the tuned Random Forest from the previous step, 
# #and redo our errors and top decile calculations for the previously identified threshold. 

# IDataset.test.balanced.data1$predict.class = predict(tRndFor, IDataset.test.balanced.data1 , type="class")

# IDataset.test.balanced.data1$prob1 = predict(tRndFor, IDataset.test.balanced.data1 , type="prob")[,"0"]

# tbl=table(IDataset.test.balanced.data1 $default, IDataset.test.balanced.data1 $predict.class)

# print('accuracy is ')
# sum(diag(tbl))/sum(tbl)

# pred_ROCR <- prediction(IDataset.test.balanced.data1 $prob1, IDataset.test.balanced.data1 $default)
# roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
# plot(roc_ROCR, main = "ROC curve", colorize = T)
# abline(a = 0, b = 1)

# auc_ROCR <- performance(pred_ROCR, measure = "auc")
# auc_ROCR <- auc_ROCR@y.values[[1]]


# print('AUC is ')
# auc_ROCR





# #result
# #train accuracy = 0.93
# #test accuracy = 0.94






## Split the data into train & test dataset. Split80:20
seed = 101

set.seed(seed)
index = sample(1:nrow(IDataset),0.80*nrow(IDataset))
IDataset.train2 = IDataset[index,]
IDataset.test2 = IDataset[-index,]





## Let's check the count of unique value in the target variable
as.data.frame(table(IDataset.train2$default))

## Let's check the count of unique value in the target variable
as.data.frame(table(IDataset.test2$default))





# let us build the model with all varaibles

LRmodel = glm(default~ Income+Count_3_6_months_late+Count_6_12_months_late+Count_more_than_12_months_late+risk_score+age_in_years+perc_premium_paid_by_cash_credit , data = IDataset.train2, family= binomial)
summary(LRmodel)

# Using stepwise algorithm for removing insignificant variables

library(MASS)
log_model = stepAIC(LRmodel, direction = "both",k=5) 
summary(log_model)


# lets see important variables

library(caret)
varImp(log_model)

# convert to data frame
l = data.frame(varImp(log_model))
l <- cbind(newColName = rownames(l), l)
rownames(l) <- 1:nrow(l)

# soritng the imprtance of varaible 
l[with(l, order(-Overall)), ]

# lets see model performances on train data
# prediction on test dataset
IDataset.train2$prob1 = predict(log_model, newdata= IDataset.train2, type="response")
tb.train2 = table(IDataset.train2$prob1>0.50, IDataset.train2$default)
print('accuracy is ')
sum(diag(tb.train2))/sum(tb.train2)



confusionMatrix(as.factor(as.integer(as.logical(IDataset.train2$prob1>0.50))), IDataset.train2 $default, dnn = c("Prediction", "Reference"))


trainsensitivity2 = tb.train2[2,2] / sum(tb.train2[2, ])
trainsensitivity2

trainspecificity2 = tb.train2[1,1] / sum(tb.train2[1, ])
trainspecificity2

# let us check accuracy on test data set
# prediction on test dataset
IDataset.test2$prob1 = predict(log_model, newdata= IDataset.test2, type="response")
tb.test2 = table(IDataset.test2$prob1>0.50, IDataset.test2$default)
print('accuracy is ')
sum(diag(tb.test2))/sum(tb.test2)



confusionMatrix(as.factor(as.integer(as.logical(IDataset.test2$prob1>0.50))), IDataset.test2 $default, dnn = c("Prediction", "Reference"))




testsensitivity2 = tb.test2[2,2] / sum(tb.test2[2, ])
testsensitivity2

testspecificity2 = tb.test2[1,1] / sum(tb.test2[1, ])
testspecificity2

























library(hrbrthemes)


pie(IDataset$Count_3_6_months_late)

# create dummy data
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5)
)

# Stacked density plot:
ggplot(data=IDataset, aes(x=Count_3_6_months_late, group=Count_3_6_months_late, fill=Count_3_6_months_late)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()

ggplot(IDataset,
       aes(x = Count_3_6_months_late,
           fill = default)) +
  labs(fill = "default", # setting title of legend
       x = "Count_3_6_months_late") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked

ggplot(IDataset,
       aes(x = default,
           fill = Count_6_12_months_late)) +
  labs(fill = "Count_6_12_months_late", # setting title of legend
       x = "default") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked


ggplot(IDataset,
       aes(x = default,
           fill = Count_more_than_12_months_late)) +
  labs(fill = "Count_more_than_12_months_late", # setting title of legend
       x = "default") +
  geom_bar(position = "stack") #specifying the type of bar chart as stacked
#result
#train accuracy = 94
#train sensitivity = 94
#train specificity = 57


#test accuracy = 93
#train sensitivity = 94
#train specificity = 51



















