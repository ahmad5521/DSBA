### Program: PGP-DSBA
### Batch: January 2020
### Module: Machine Learning
### Project3: Thera Bank (Loan Purchase Modelling) Project
### Great Learning




#### ENVIRONMENT SET-UP & DATA IMPORT


### Set working directory to read required input data and save any output
setwd("D:/thera bank solution new/vinod 60/")
getwd() 


### Load installed package "readxl" to import .xlsx file (given) into the global environment
library(readxl)


### Import given data in .xlsx (in second sheet) format into the global environment
rdata <- read_xlsx("Thera Bank_Personal_Loan_Modelling-dataset.xlsx", sheet = 2)
# Data has been imported and saved as "rdata"
# It is apparent that rdata dataset contains 5000 observations (rows) and 14 variables (columns)





#### EXPLORATORY DATA ANALYSIS (EDA)

### UNDERSTANDING THE DATA

## Data structure
# Structure
str(rdata)

# Load DataExplorer for exploratory data analysis. 
library(DataExplorer)

# This function helps to visualize data structure in network graph format.
plot_str(rdata, type="d", fontSize = 25)
# It is apparent that the dataset is a dataframe and contains 5000 observations (rows) and 14 variables (columns)
# All the variables are in the number format. Some variables are to be converted as factor / integer as required.

# Check the fivenumber summary of variable "Income"
summary(fivenum(rdata$`Income (in K/month)`))
# Mean/Median Indicates that this is the annual income



## Check for missing values
plot_missing(rdata)
# Variable Family Members has 0.36% missing values

# Check the fivenumber summary of variable "Family members"
summary(fivenum(rdata$`Family members`))
# missing values could be replaced with the median value



## Initial Summary of the given dataset
summary(rdata)

########## some keys observations from summary ########
# average age of customers is 45 years
# customers on an average has experience of 20 years
# there is gap in median and mean of income indicating that distribution is skewed
# only 480 customers were given loan out of 5000 customers

####### Data manipulation (Convert data types & remove unwanted variables)

# create a copy of rdata
cdata <- rdata


# remove variable "ID" because it is not significant in model building
cdata <- cdata[,-1]
# Check if the variable "ID" is removed
names(cdata)




# Convert ZIP Code, Education, Personal loan, Securities account, CD account, Online and Credit Card as factor
fac <- c(4,7,9:13)
cdata[,fac] <- lapply(cdata[,fac], as.factor)


# Rename numeric values of Education (ordinal data)
levels(cdata$Education)
summary(cdata$Education)
cdata$Education = factor(cdata$Education, labels = c("Undergrad", "Graduate", "Adv/Professional"), order = TRUE)
summary(cdata$Education)

# Check the column names and make them syntactically valid
##colnames(cdata) = make.names(colnames(cdata))

names(cdata) <- gsub(" ", "_", names(cdata))
names(cdata)[1] <- "Age"
names(cdata)[2] <- "Experience"
names(cdata)[3] <- "Income_year"
names(cdata)[6] <- "CCAvg_month"


colnames(cdata)


# This function helps to visualize data structure in network graph format.
plot_str(cdata, type="d", fontSize = 25)


# Missing values treatment
# Find missing values by column
colSums(is.na(cdata))
# there are 18 missing values under variable Family_members

# Check the fivenumber summary
summary(fivenum(cdata$Family_members))
# Median is a good replacement as mean of 2.2 doesnt make sense

# Replace all the missing cells in Family Members with the median value
cdata$Family_members[is.na(cdata$Family_members)] = median(cdata$Family_members, na.rm = T)
colSums(is.na(cdata))
# All missing values are imputed.




# Check negative values
exp_neg = subset(cdata, cdata$Experience < 0)
length(exp_neg$Experience)
# there are 52 negative values
exp_neg$Experience



# consider negatives values as missing values and use mice package to impute
cdata[cdata$Experience<0,'Experience'] = NA
library(mice)

imputed_data = mice(cdata, m=1, maxit = 50, method = 'pmm', seed = 500)
#check imputed values
imputed_data$imp$Experience
#get complete data 
cdata = complete(imputed_data,1)








# Unique values in ZIP Code
length(unique(cdata$ZIP_Code))

# Create a new variable with the first 2 digits of ZIP Code
cdata$ZIP = substr(cdata$ZIP_Code,1,2)
cdata$ZIP = as.factor(cdata$ZIP)
names(cdata)

# Unique values in ZIP 
length(unique(cdata$ZIP))

# remove ZIP_Code
cdata <- cdata[,-4]


# let us see the manipulated structure of data
str(cdata)


# Plot to get introduced to the newly created dataset
plot_intro(cdata, geom_label_args = list("size" = 3.5))

# data cleaning complete


# Check the summary of the dataset
summary(cdata)






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
g1=ggplot(cdata, aes(x=Education, fill=Education)) + geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g2=ggplot(cdata, aes(x=Personal_Loan, fill = Personal_Loan))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g3=ggplot(cdata, aes(x=Securities_Account, fill = Securities_Account))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g4=ggplot(cdata, aes(x=CD_Account, fill = CD_Account))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g5=ggplot(cdata, aes(x=Online, fill = Online))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g6=ggplot(cdata, aes(x=CreditCard, fill = CreditCard))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))
  
g7=ggplot(cdata, aes(x=ZIP, fill = ZIP))+ geom_bar()+ unipar + scale_fill_brewer(palette=col1) +
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", size = 3.3, position = position_stack(0.06))+
  geom_text(aes(label = ..count.., group = 1), stat= "count", size = 3.3, position = position_stack(0.95))


# Partitioning the barcharts
grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3)

#################### key observations   #################################################

##** only 10% of customers were given loan
##** only 10% customers had security account while 90 % didnot
##** majority customers (94%) do not have CD account
##** majority customers (60%) have availed online services 
##** majority customers have not availed credit card services


colnames(cdata)
## visualize properties of all continuous variables


par(mfrow = c(3,2)); 


text(x= barplot(table(cdata$Age),col='#69b3a2', main = "Age",ylab = "Frequency"), 
     y = 0, table(cdata$Age), cex=1,pos=1); 
boxplot(cdata$Age, col = "steelblue", horizontal = TRUE, main = "Age"); 
text(x = fivenum(cdata$Age), labels = fivenum(cdata$Age), y = 1.25)

text(x= barplot(table(cdata$Experience),col='#69b3a2', main = "Experience",ylab = "Frequency"), 
     y = 0, table(cdata$Experience), cex=1,pos=1); 
boxplot(cdata$Experience, col = "steelblue", horizontal = TRUE, main = "Experience"); 
text(x = fivenum(cdata$Experience), labels = fivenum(cdata$Experience), y = 1.25)

text(x= barplot(table(cdata$Income_year), col='#69b3a2', main = "Income(K$/year)",ylab = "Frequency"), 
     y = 0, table(cdata$Income_year), cex=1,pos=1); 
boxplot(cdata$Income_year, col = "steelblue", horizontal = TRUE, main = "Income(K$/year)");
text(x = fivenum(cdata$Income_year), labels = fivenum(cdata$Income_year), y = 1.25)



#################### key observations   #################################################

##** Age is normally distributed with medain around 45. 
##** middle 50% customers are aged between 35 years and 55 years
##** Experience is also noramlly distributed. 
##** 50% people have work experience between 11 years and 30 years
##** Income is skewed towards right.
##** 50% customers have income between 39k to 98K while highest income is 224K which is as outlier







par(mfrow = c(3,2)); 

text(x= barplot(table(cdata$Family_members), col='#69b3a2', main = "Family Members",ylab = "Frequency"), 
     y = 0, table(cdata$Family_members), cex=1,pos=1); 
boxplot(cdata$Family_members, col = "steelblue", horizontal = TRUE, main = "Family Members");
text(x = fivenum(cdata$Family_members), labels = fivenum(cdata$Family_members), y = 1.25)
  
text(x= barplot(table(cdata$CCAvg_month), col='#69b3a2', main = "CC Avg_month(K$/mth)",ylab = "Frequency"), 
     y = 0, table(cdata$CCAvg_month), cex=1,pos=1); 
boxplot(cdata$CCAvg_month, col = "steelblue", horizontal = TRUE, main = "CC Avg_month(K$/mth)");
text(x = fivenum(cdata$CCAvg_month), labels = fivenum(cdata$CCAvg_month), y = 1.25)

text(x= barplot(table(cdata$Mortgage), col='#69b3a2',ylim=c(0,20), main = "Mortgage(K$)",ylab = "Frequency"), 
     y = 0, table(cdata$Mortgage), cex=1,pos=1); 
boxplot(cdata$Mortgage, col = "steelblue", horizontal = TRUE, main = "Mortgage(K$)")
text(x = fivenum(cdata$Mortgage), labels = fivenum(cdata$Mortgage), y = 1.25)


#################### key observations   #################################################

##** Majority customers had 1 or 2 or 3 family members 
##** CC_Avg is highly skewed towards right. 
##** 50% customers have Credit card monthly spend below 1.5K 
##** There are some outliers whose credit spen per month is more than 5K 
##** Mortage is also skewed toward right





### BIVARIATE ANALYSIS


# Setting up the aesthetics
bipar1 = theme(legend.position = "none") + theme_light() +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))

# Define color brewer
col2 = "Set2"

# Personal Loan vs numerical variables
p1=ggplot(cdata, aes(x = Personal_Loan, y = Age, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
   stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()

p2=ggplot(cdata, aes(x = Personal_Loan, y = Experience, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
  stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()

p3=ggplot(cdata, aes(x = Personal_Loan, y = Income_year, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
  stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()

p4=ggplot(cdata, aes(x = Personal_Loan, y = Family_members, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
  stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()

p5=ggplot(cdata, aes(x = Personal_Loan, y = CCAvg_month, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
  stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()

p6=ggplot(cdata, aes(x = Personal_Loan, y = Mortgage, fill = Personal_Loan)) + geom_boxplot(show.legend = FALSE)+ bipar1 + scale_fill_brewer(palette=col2) +
  stat_summary(fun = quantile, geom = "text", aes(label=sprintf("%1.0f", ..y..)),position=position_nudge(x=0.5), size=4, color = "black") + coord_flip()


# Partitioning the boxplots
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)

#################### key observations   #################################################

##** Majority of people who had taken personal loan have higher income than those who did not,
#       take loan which indicates that customers with higher income have higher liklihood of getting loan.
##** customers who had larger family size have higher liklihood of taking loan
##** Customers who had higher Credit card average spend per month have higher liklihood of taking loan.







# Setting up the aesthetics
bipar2 = theme(legend.position = "top", 
               legend.direction = "horizontal", 
               legend.title = element_text(size = 10),
               legend.text = element_text(size = 8)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13, face = "bold"))


library(dplyr)

# Personal Loan vs categorical variables
d8 <- cdata %>% group_by(Education) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p8=ggplot(cdata, aes(x=Education, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d8, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d9 <- cdata %>% group_by(Securities_Account) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p9=ggplot(cdata, aes(x=Securities_Account, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d9, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d10 <- cdata %>% group_by(CD_Account) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p10=ggplot(cdata, aes(x=CD_Account, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d10, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d11 <- cdata %>% group_by(Online) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p11=ggplot(cdata, aes(x=Online, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d11, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d12 <- cdata %>% group_by(CreditCard) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p12=ggplot(cdata, aes(x=CreditCard, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d12, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

d13 <- cdata %>% group_by(ZIP) %>% count(Personal_Loan) %>% mutate(ratio=scales::percent(n/sum(n)))
p13=ggplot(cdata, aes(x=ZIP, fill=Personal_Loan)) + geom_bar()+ bipar2 + scale_fill_brewer(palette=col2) +
  geom_text(data=d13, aes(y=n,label=ratio),position=position_stack(vjust=0.5))

# Partitioning the boxplots
grid.arrange(p8,p9,p10,p11,p12,p13,ncol=3)

#################### key observations   #################################################

##** As education level among customers increases their likihood of taking personal loan also increases
#       . 14% professional had taken loans
##** 46% of those customers who had CD account had taken loan indicating that 
#           customers with CD account have higher liklihood of taking loan.




## numerical vs numerical variables

# Create a subset of data with only the numeric variables
subset_cdata = cdata[, c("Age","Experience","Income_year","Family_members", "CCAvg_month", "Mortgage")] 


# Plot the correlation plot of numeric variables
library(corrplot)

par(mfrow = c(1,1));
corrplot(cor(subset_cdata), method = "number", type = "upper")

#################### key observations   #################################################

##** Income of customers is positively correlated with CC Average per month


# Bivariate analysis of numerical variables with strong correlation

ggplot(cdata, aes(x = Income_year, y = CCAvg_month)) + bipar2 + scale_fill_brewer(palette=col2) +
  geom_jitter(alpha = 0.8, color = "lightskyblue3", fill = "black", size = 3)+
  geom_smooth(method = lm, se=FALSE, color = "red")+
  labs(x = "Income/year", y = "Credit Card Spend", title = "Income vs CC Spend")


# observation
##** Income of customers is positively correlated with CC Average per month






#### CLUSTERING

### Get Data 

clustdata <- cdata



## Outlier analysis
# Outliers identified in Income, CCAvg & Mortgage
## Income:
# Upper fence of Income
quantile(clustdata$Income_year,0.75)+(1.5*(IQR(clustdata$Income_year)))
# count of outlier values
sum(clustdata$Income_year > quantile(clustdata$Income_year,0.75)+(1.5*(IQR(clustdata$Income_year))))
# proportion of outliers
(sum(clustdata$Income_year > quantile(clustdata$Income_year,0.75)+(1.5*(IQR(clustdata$Income_year)))) / length(clustdata$Income_year))*100
# 1.92%


## CCAvg:
# Upper fence of CCAvg
quantile(clustdata$CCAvg_month,0.75)+(1.5*(IQR(clustdata$CCAvg_month)))
# count of outlier values
sum(clustdata$CCAvg_month > quantile(clustdata$CCAvg_month,0.75)+(1.5*(IQR(clustdata$CCAvg_month))))
# proportion of outliers
(sum(clustdata$CCAvg_month > quantile(clustdata$CCAvg_month,0.75)+(1.5*(IQR(clustdata$CCAvg_month)))) / length(clustdata$CCAvg_month))*100
# 6.48%


## Mortgage:
# Upper fence of Mortgage
quantile(clustdata$Mortgage,0.75)+(1.5*(IQR(clustdata$Mortgage)))
# count of outlier values
sum(clustdata$Mortgage > quantile(clustdata$Mortgage,0.75)+(1.5*(IQR(clustdata$Mortgage))))
# proportion of outliers
(sum(clustdata$Mortgage > quantile(clustdata$Mortgage,0.75)+(1.5*(IQR(clustdata$Mortgage)))) / length(clustdata$Mortgage))*100
# 5.82%


## Check data
library(psych)
describe(clustdata,na.rm = TRUE,quant = c(0.01,0.05,0.10,0.25,0.75,0.90,0.95,0.99),IQR=TRUE,check=TRUE)




# converting education to integer type data for clsutering
clustdata$Education          = as.integer(clustdata$Education)
clustdata$Securities_Account =  as.numeric(levels(clustdata$Securities_Account))[clustdata$Securities_Account]
clustdata$CD_Account         =  as.numeric(levels(clustdata$CD_Account))[clustdata$CD_Account]
clustdata$Online             =  as.numeric(levels(clustdata$Online))[clustdata$Online]
clustdata$CreditCard         =  as.numeric(levels(clustdata$CreditCard ))[clustdata$CreditCard ]

# removing Personal loan (target) and zip as not needed for clustering
clustdata[,c("Personal_Loan","ZIP")] = NULL


### Scaling data
clustdata_scaled = scale(clustdata)
summary(clustdata_scaled)
describe(clustdata_scaled,na.rm = TRUE,quant = c(0.01,0.05,0.10,0.25,0.75,0.90,0.95,0.99),IQR=TRUE,check=TRUE)


# Post scaling mean must be 0 and sd 1
describe(clustdata_scaled,na.rm = TRUE,quant = c(0.01,0.05,0.10,0.25,0.75,0.90,0.95,0.99),IQR=TRUE,check=TRUE)
apply(clustdata_scaled,2,mean)
apply(clustdata_scaled,2,sd)





### Hierarchical Clustering

# create a copy of clustdata & clustdata_scaled
hcdata <- clustdata
hcdata_scaled <- clustdata_scaled
colnames(hcdata_scaled)


# Compute distance matrix with scaled data
distmat = dist(hcdata_scaled, method = "minkowski", p=1)
print(distmat, digits = 3)



# Hierarchical clustering using average method
# Hierarchial clustering with ward.D2 method
hcluster = hclust(distmat, method =  "ward.D2")
print(hcluster)



# plot the dendrogram

par(mfrow = c(1,1));
plot(hcluster, hang = -1, cex = 0.6, col = "steelblue")


# Plot rectangles defining the clusters for given level of K=6
# reading from dendogram apt number of cluster is 6
plot(hcluster, hang = -1, cex = 0.6, col = "steelblue")
rect.hclust(hcluster, k = 6, border = "red")


# write the clusters 1&2 back into the original dataset
hcdata$cluster = cutree(hcluster, k=6)

# No of observations in each cluster
table(hcdata$cluster)


# Aggregate columns for each cluster by their means
hc_custprofile = aggregate(hcdata[,-13], list(hcdata$cluster), FUN = "mean", simplify = TRUE)
print(hc_custprofile)


#### Creating summary table using rpivotTable function
library(rpivotTable)
rpivotTable(hcdata)

# using rpivottable we can derive following observation regarding clusters

#################### key observations   #################################################

##### cluster 1 ###################################
# there are 373 observation
# majority 100% have securty account
# Cerdit card avg spen is very low

##### cluster 2  ###################################
# This cluster have higest number of observations (1685)
# This cluster has medaian age very high 54 years
# This clustr have very high work experience (median work experience is 29 years)
# majority have 1 or 2 family members

##### cluster 3  ###################################

# there are 1044 observarion
# Medain age lowest (34 yrs) compared to all other cluster customers ages
# this cluster has only 49% customer availing online service which is lowest compartively
# no credit card user in this clusters

##### cluster 4  ###################################

# All customers in this cluster is ceredit card users
# There are 1059 customers
# all customers in this clusters are credit card holders

##### cluster 5  ###################################

# there are 537 customers in this clusters
# The median income of this clsuer is highest which is 140k
# In this clusers of customers, less than 3% have family member of size 4
# majority are undergrad
# This clusters have highest mortgage (avergae mortgage is 117.73)
# Credit card avg spend is higest 

##### cluster 6  ###################################

# there are 302 customers in this clusters
# This cluster has high income customers (median income is 107k)
# credit card avg spend is also high 
# mortgage is high
# half (49%) have security account
# All customers are CD account holders
# 94% customer have availed online services which highest comparatively
# 79 % are credit card holder









######################################
#  K means Clustering
######################################
# create a copy of clustdata & clustdata_scaled

kmdata <- clustdata
kmdata_scaled <- clustdata_scaled
summary(kmdata_scaled)
colnames(kmdata_scaled)

library(factoextra)
seed = 101
set.seed(seed)

## Find optimal no of clusters - Elbow method
elb = fviz_nbclust(kmdata_scaled, kmeans, method = "wss",k.max = 15,print.summary = TRUE) + 
  geom_vline(xintercept = 10, linetype = 2) + theme_minimal() + ggtitle("The Elbow Method")

elb
# after k=10, the curve flattens

## Find optimal no of clusters - Silhoutte method

sil = fviz_nbclust(kmdata_scaled, kmeans, method = "silhouette",k.max = 15, print.summary = TRUE) + 
  theme_minimal() + ggtitle("The Silhouette method")
sil

# sil indicates 7 is the optimal number of clusters

## Find optimal no of clusters - Gap statistic method
set.seed(seed)

# indicates 10 is the optimal number of clusters

library(gridExtra)
# Partitioning the plots
grid.arrange(elb,sil, ncol=1)
# this suggest that number of cluster is either  7 or 15, we will choose 7 




## K means clustering with best k=7
seed = 101
set.seed(seed)
kmeanscluster = kmeans(x = kmdata_scaled, centers = 7, nstart = 25)
print(kmeanscluster)




## Add the cluster number column back in our original dataset
kmdata$Cluster <- kmeanscluster$cluster
head(kmdata)


# No of observations in each cluster
table(kmdata$Cluster)



## Aggregate columns for each cluster by their means
km_custprofile = aggregate(kmdata[,-13], list(kmdata$Cluster), FUN = "mean", simplify = TRUE)
print(km_custprofile)


#### Creating summary table using rpivotTable function

rpivotTable(kmdata)


#################### key observations with Kmeans    #################################################

##** clusters 2 and 7  has highest numers of customers while clusters 6 have only 302 customers which is lowest

##** cluster 1 has high income custers (ther are 596 customers)

##** clusters 2 has very matur customers median aged 56 

##** In cluster 7 all customers availed online services while in cluster 3 none
##** all customers in cluster 5 had securities account while in cluster 6 49% customers has securities account rest clusters has almost no customers with securities account
##** cluster 1 and 6 has highest mortagea (avergare is above 90)





#### CART

## Get data
cartdata <- cdata
summary(cartdata)
str(cartdata)

## Split the data into train & test dataset. Split70:30
seed = 101

set.seed(seed)
index = sample(1:nrow(cartdata),0.70*nrow(cartdata))
carttrain = cartdata[index,]
carttest = cartdata[-index,]

dim(carttrain)
dim(carttest)


## Required libraries
library(rpart)
library(rpart.plot)
library(rattle)


# Define CART control parameters
# minsplit is " the min number of observations that must exist in a node for a split"
# default is rpart.control(minsplit = 20, minbucket = round(minsplit/3)
# minbucket is "the minimum number of observations in any terminal node"
# complexity parameter (cp) = 0 (to create a very complex tree (overfit))
r.ctrl = rpart.control(minsplit = 10, minbucket = 3, cp = 0)

## Build the CART model
set.seed(seed)
tree = rpart(formula = Personal_Loan ~., data = carttrain, method = "class", control = r.ctrl)
tree


# Visualize the decision tree
rpart.plot(tree, tweak = 2.0)
fancyRpartPlot(tree, tweak = 3.5 , main = "Decision Tree", type = 4)


# Print the complexity chart
printcp(tree)


# Plotcp() provides a graphical representation to the cross validated error summary. 
plotcp(tree)


## Pruning the tree
ptree = prune(tree, cp = 0.012, "CP")
printcp(ptree)





# Visualize the pruned decision tree
fancyRpartPlot(ptree, tweak = 2.2 , main = "Pruned Decision Tree", type = 4)


# Finally after the tree is pruned
# We want to use that tree to make predictions on the train & test dataset
carttrain$Prediction = predict(ptree, carttrain, type = "class")
carttrain$Score = predict(ptree, carttrain, type = "prob")[,"1"]

carttest$Prediction = predict(ptree, carttest, type = "class")
carttest$Score = predict(ptree, carttest, type = "prob")[,"1"]


# Find the % of Personal_Loan = 1
sum(carttrain$Personal_Loan ==1)/nrow(carttrain) # baseline model has 91 % accuracy
sum(carttrain$Prediction ==1)/nrow(carttrain)

sum(carttest$Personal_Loan ==1)/nrow(carttest)
sum(carttest$Prediction ==1)/nrow(carttest)


# Compute the confusion matrix
table(carttrain$Personal_Loan, carttrain$Prediction)
(13+31)/3500
# error rate 1.2% on train

table(carttest$Personal_Loan, carttest$Prediction)
(7+17)/1500
# error rate 1.6 %

# our tree has generalized well on test data

# ROC, AUC, KS, Gain 

cartpredobj_train <- prediction(carttrain$Score, carttrain$Personal_Loan)
cartpredobj_test <- prediction(carttest$Score, carttest$Personal_Loan)

## ROC Curve
# Training data
cartperf_train <- performance(cartpredobj_train, "tpr", "fpr")
plot(cartperf_train)
abline(0, 1, lty = 2)


# Testing data
cartperf_test <- performance(cartpredobj_test, "tpr", "fpr")
plot(cartperf_test)
abline(0, 1, lty = 2)

## Compute AUC
cartauctrain = performance(cartpredobj_train, "auc")
cartauctrain = as.numeric(cartauctrain@y.values)
print(cartauctrain)

# with tree built using all varaibles AUC is 0.982 on train data set

cartauctest = performance(cartpredobj_test, "auc")
cartauctest = as.numeric(cartauctest@y.values)
print(cartauctest)

# with tree built using 6 varaibles AUC is 0.984 on train


## Compute KS
max(cartperf_train@y.values[[1]]-cartperf_train@x.values[[1]])
# .92 on train
max(cartperf_test@y.values[[1]]-cartperf_test@x.values[[1]])
# 0.91

## Compute Gini
cartginitrain = ineq(carttrain$Score, "gini")
print(cartginitrain)

cartginitest = ineq(carttest$Score, "gini")
print(cartginitest)


## Concordant-Discordant ratio
Concordance(actuals = carttrain$Personal_Loan, predictedScores = carttrain$Score)

Concordance(actuals = carttest$Personal_Loan, predictedScores = carttest$Score)







#### RANDOM FOREST

### Get data
rfdata <- cdata
summary(rfdata)

## Split the data into train & test dataset. Split70:30
seed = 101
set.seed(seed)

index = sample(1:nrow(rfdata),0.70*nrow(rfdata))
rftrain = rfdata[index,]
rftest = rfdata[-index,]

dim(rftrain)
dim(rftest)


## Required libraries
library(randomForest)


# Define RF control parameters:
# ntree- no of trees to be built (ensure to give an odd number)
# mtry- no of independent variables that we will randomly pick everytime for branching
# nodesize-node size of a terminal branch
# importance-how important each of the independent variables were


### Build the Random Forest

set.seed(seed)
rndforest = randomForest(Personal_Loan ~ ., data = rftrain, ntree = 501, mtry = 4, nodesize = 10, importance = TRUE)
print(rndforest)


## Plot the random forest
plot(rndforest)
# We see that above 50 trees (black line), the OOB error rate is not reducing
# So lets stick to 51 trees 


# Look at the top 5 important variables
varImpPlot(rndforest, sort = T, main = "Variable Importance Plot", n.var = 6)

###
# 1 Income Year
# 2 Education
# 3 Family Members
# 4 CC Avg month
# 5 CD account
# 6 Experience

# lest make tree to derive buisness rules

## Build the CART model 6 important variables
set.seed(seed)
r.ctrl = rpart.control(minsplit = 10, minbucket = 5, cp = 0)
tree = rpart(formula = Personal_Loan ~Income_year+Education+Family_members+CCAvg_month+CD_Account+Experience, data = carttrain, method = "class", control = r.ctrl)
tree
# Visualize the decision tree
rpart.plot(tree, tweak = 2.0)
fancyRpartPlot(tree, tweak = 3.5 , main = "Decision Tree", type = 4)

# Print the complexity chart
printcp(tree)
# Plotcp() provides a graphical representation to the cross validated error summary. 
plotcp(tree)


## Pruning the tree
ptree = prune(tree, cp = 0.011, "CP")
printcp(ptree)

# Visualize the pruned decision tree
fancyRpartPlot(ptree, tweak = 2.2 , main = "Pruned Decision Tree", type = 4)

# We want to use that tree to make predictions on the train & test dataset
carttrain$Prediction_br = predict(ptree, carttrain, type = "class")
carttrain$Score_br = predict(ptree, carttrain, type = "prob")[,"1"]

carttest$Prediction_br = predict(ptree, carttest, type = "class")
carttest$Score_br = predict(ptree, carttest, type = "prob")[,"1"]

# Compute the confusion matrix
table(carttrain$Personal_Loan, carttrain$Prediction_br)
(14+27)/3500 # error rate is 1.1%
table(carttest$Personal_Loan, carttest$Prediction_br)
(16+8)/1500 # error rate on test is 1.6

# so tree accuracy improved on train data set after using just 6 varaible while
# on test accuracy reamins the same as old model


cartpredobj_train <- prediction(carttrain$Score, carttrain$Personal_Loan)
cartpredobj_test <- prediction(carttest$Score, carttest$Personal_Loan)


## ROC Curve
# Training data
cartperf_train <- performance(cartpredobj_train, "tpr", "fpr")
plot(cartperf_train)
abline(0, 1, lty = 2)


# Testing data
cartperf_test <- performance(cartpredobj_test, "tpr", "fpr")
plot(cartperf_test)
abline(0, 1, lty = 2)

## Compute AUC
cartauctrain = performance(cartpredobj_train, "auc")
cartauctrain = as.numeric(cartauctrain@y.values)
print(cartauctrain)

# with tree built using all varaibles AUC is 0.982 on train

cartauctest = performance(cartpredobj_test, "auc")
cartauctest = as.numeric(cartauctest@y.values)
print(cartauctest)

# with tree built using 6 varaibles AUC is 0.984 on train


## Compute KS
max(cartperf_train@y.values[[1]]-cartperf_train@x.values[[1]])

max(cartperf_test@y.values[[1]]-cartperf_test@x.values[[1]])





## Compute Gini
cartginitrain = ineq(carttrain$Score, "gini")
print(cartginitrain)

cartginitest = ineq(carttest$Score, "gini")
print(cartginitest)


## Concordant-Discordant ratio
Concordance(actuals = carttrain$Personal_Loan, predictedScores = carttrain$Score)

Concordance(actuals = carttest$Personal_Loan, predictedScores = carttest$Score)








# Tuning Control parameters
# x = all independent variables (predictor variables)
# y = dependent variable (response variable)
# mtryStart = no of variable to consider for the split (starting value of mtry, default is same as in RF)
# stepfactor = decides various no of variables(m) by multiplying or dividing by the stepfactor
# ntreeTry = no of trees used at the tuning step
# improve =the (relative) improvement in OOB error must be by this much for the search to continue
# trace = whether to print the progress of the search
# dobest = whether to run a forest using the optimal mtry found

# Tuning the random forest
set.seed(seed)

trndforest = tuneRF(x=rftrain[,-8], y = rftrain$Personal_Loan, mtryStart = 4, stepFactor = 1.5, 
                    ntreeTry = 51, improve = 0.0001, nodesize = 10, trace = TRUE, plot = TRUE, doBest = TRUE, importance = TRUE)

# returned a random forest with m = 9 and ntreeTry = 51



# After tuning the RF
# We want to use that tree to make predictions on the train & test dataset
rftrain$Prediction = predict(trndforest, rftrain, type = "class")
rftrain$Score = predict(trndforest, rftrain, type = "prob")[,"1"]

rftest$Prediction = predict(trndforest, rftest, type = "class")
rftest$Score = predict(trndforest, rftest, type = "prob")[,"1"]



# Find the % of Personal_Loan = 1
sum(rftrain$Personal_Loan ==1)/nrow(rftrain)
sum(rftrain$Prediction ==1)/nrow(rftrain)

sum(rftest$Personal_Loan ==1)/nrow(rftest)
sum(rftest$Prediction ==1)/nrow(rftest)


# Compute the confusion matrix
table(rftrain$Personal_Loan, rftrain$Prediction)
(2+21)/3500 # error rate .6% on tran data set
table(rftest$Personal_Loan, rftest$Prediction)
(6+17)/1500 # error rate is 1.5% on test data set which is better than Tree model

#

rfpredobj_train <- prediction(rftrain$Score, rftrain$Personal_Loan)
rfpredobj_test <- prediction(rftest$Score, rftest$Personal_Loan)

## ROC Curve
# Training data
rfperf_train <- performance(rfpredobj_train, "tpr", "fpr")
plot(rfperf_train)
abline(0, 1, lty = 2)

# Testing data
rfperf_test <- performance(rfpredobj_test, "tpr", "fpr")
plot(rfperf_test)
abline(0, 1, lty = 2)

## Compute AUC
rfauctrain = performance(rfpredobj_train, "auc")
rfauctrain = as.numeric(rfauctrain@y.values)
print(rfauctrain)

# AUC on train is 0.999

rfauctest = performance(rfpredobj_test, "auc")
rfauctest = as.numeric(rfauctest@y.values)
print(rfauctest)

# AUC on test data set with RF is 0.998

#  RF model is btter than Tree


## Compute KS
max(rfperf_train@y.values[[1]]-rfperf_train@x.values[[1]])

max(rfperf_test@y.values[[1]]-rfperf_test@x.values[[1]])



## Compute Gini
rfginitrain = ineq(rftrain$Score, "gini")
print(rfginitrain)

rfginitest = ineq(rftest$Score, "gini")
print(rfginitest)


## Concordant-Discordant ratio
Concordance(actuals = rftrain$Personal_Loan, predictedScores = rftrain$Score)

Concordance(actuals = rftest$Personal_Loan, predictedScores = rftest$Score)











### IDENTIFY POTENTIAL CUSTOMERS


## Let us use the random forest TRAIN dataset
rfdecile_train = rftrain

# Chop all probabilities into 10 different buckets
qs_train = quantile(rfdecile_train$Score, prob = seq(0,1,length = 11))
print(qs_train)
qs_train[10]
# Top 10% of all the probabilities lie b/w 0.2462 & 1.0000 (those who responded)

threshold_train = qs_train[10]
mean(rfdecile_train$Personal_Loan[rfdecile_train$Score>threshold_train] =="1")

# If we launch campaign targetting people with prob > 0.2462,
# 95.71 % of them have target = 1 or will respond


# Let us check the response rate at a threshold of 70%
threshold_70 = 0.7
mean(rfdecile_train$Personal_Loan[rfdecile_train$Score>threshold_70] =="1")
# 100 % of them have target = 1 or will respond







## Let us use the random forest TEST dataset
rfdecile_test = rftest

# Chop all probabilities into 10 different buckets
qs_test = quantile(rfdecile_test$Score, prob = seq(0,1,length = 11))
print(qs_test)
qs_test[10]
# Top 10% of all the probabilities lie b/w 0.2462 & 1.0000 (those who responded)

threshold_test = qs_train[10]
threshold_train
# Using the same threshold as that of the training data
mean(rfdecile_test$Personal_Loan[rfdecile_test$Score>threshold_train] =="1")

# If we launch campaign targetting people with prob > 0.2462,
# 90.67 % of them have target = 1 or will respond



# Let us check the response rate at a threshold of 70%
threshold_70 = 0.7
mean(rfdecile_test$Personal_Loan[rfdecile_test$Score>threshold_70] =="1")
# 98.3 % of them have target = 1 or will respond






